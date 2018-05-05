{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}


module Main where

import           Config

import           Control.Concurrent              (forkIO)
import           Control.Concurrent.STM          (TMVar, atomically,
                                                  newEmptyTMVarIO, newTMVarIO,
                                                  putTMVar, takeTMVar)
import           Control.Exception               (SomeException, handle)
import           Control.Monad                   (foldM, mzero)
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Control.Monad.State             as ST
import           Control.Monad.State.Class
import           Data.Aeson                      (FromJSON, ToJSON, Value (..),
                                                  decode, defaultOptions,
                                                  encode, genericParseJSON,
                                                  genericToEncoding, parseJSON,
                                                  toEncoding, (.:))
import           Data.Aeson.Types                (fieldLabelModifier)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8      as BS
import           Data.Data
import           Data.Functor.Identity
import           Data.List                       (sortBy, stripPrefix)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Ord                        (comparing)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           GHC.Generics                    (Generic)
import qualified GitHub.Endpoints.Repos.Contents as R
import qualified Network.AMQP                    as A
import qualified Network.HTTP.Simple             as H
import           System.Directory
import           System.Environment              (getArgs)
import           System.FilePath
import           System.IO                       (BufferMode (..),
                                                  hSetBuffering, stdout)
import qualified System.IO.Strict                as S
import           Text.EditDistance               (defaultEditCosts,
                                                  levenshteinDistance)
import           Text.Read                       (readMaybe)
import           Text.Regex.TDFA                 ((=~))


data Input = Input
  { in_from   :: String
  , in_body   :: String
  , in_sender :: String
  } deriving (Show, Generic)

data Output = Output
  { out_target       :: String
  , out_body         :: String
  , out_message_type :: String
  } deriving (Show, Generic)

data Issue = Issue
  { i_title  :: String
  , i_state  :: String
  , i_url    :: String
  , i_author :: String
  } deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON (Object v) = do
    title <- v .: "title"
    state <- v .: "state"
    url <- v .: "html_url"
    user <- v .: "user"
    case user of
      Object o -> Issue title state url <$> o.: "login"
      _        -> mzero

  parseJSON _ = mzero

instance FromJSON Input where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> fromMaybe s (stripPrefix "in_" s) }
instance ToJSON Output where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = \s -> fromMaybe s (stripPrefix "out_" s) }

exchange :: A.ExchangeOpts
exchange = A.newExchange
  { A.exchangeName = "exchange-messages"
  , A.exchangeType = "fanout"
  , A.exchangePassive = True
  , A.exchangeDurable = True
  , A.exchangeAutoDelete = False
  }

opts' :: A.SASLMechanism -> A.ConnectionOpts
opts' auth = A.defaultConnectionOpts
  { A.coVHost = "ircbot"
  , A.coTLSSettings = Just A.TLSTrusted
  , A.coServers = [("events.nix.gsc.io", 5671)]
  , A.coAuth = [auth]
  }

main = do
  hSetBuffering stdout LineBuffering

  config <- getConfig

  server config

writeDone :: TMVar () -> IO ()
writeDone var = atomically $ putTMVar var ()

server :: Config -> IO ()
server cfg = do

    conn <- A.openConnection'' (opts' (auth cfg))
    var <- newEmptyTMVarIO
    A.addConnectionClosedHandler conn True $ writeDone var
    handle (onInterrupt conn) $ do
      putStrLn "Got connection"
      chan <- A.openChannel conn
      putStrLn "Got channel"

      (publishQueueName, _, _) <- A.declareQueue chan $ A.newQueue
        { A.queueName = "queue-publish"
        , A.queuePassive = True
        }
      putStrLn $ "Declared queue with name " ++ show publishQueueName

      (myQueue, _, _) <- A.declareQueue chan $ A.newQueue
        { A.queueName = ""
        , A.queueAutoDelete = True
        , A.queueExclusive = True
        }
      putStrLn $ "Declared my queue with name " ++ show myQueue

      A.declareExchange chan exchange
      putStrLn "Declared exchange"

      A.bindQueue chan myQueue "exchange-messages" "queue-publish"
      putStrLn "Bound queue"

      A.confirmSelect chan False
      putStrLn "disabled nowait"

      statVar <- newTMVarIO initialState
      tag <- A.consumeMsgs chan myQueue A.Ack (onMessage cfg statVar chan)
      putStrLn $ "Started consumer with tag " ++ show tag

      putStrLn $ "terminating if enter is pressed..."
      atomically $ takeTMVar var
      A.closeConnection conn
onInterrupt :: A.Connection -> SomeException -> IO ()
onInterrupt conn e = do
  putStrLn "Interrupted, closing connection"
  A.closeConnection conn

publishMessage :: A.Channel -> Output -> IO (Maybe Int)
publishMessage chan msg = do
    putStrLn $ "Sending the message " ++ show msg
    -- publish a message to our new exchange
    intMb <- A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = encode msg
      , A.msgDeliveryMode = Just A.Persistent}

    putStrLn $ "Published Message" ++ maybe "" (\s -> ", got sequence number " ++ show s) intMb
    return intMb

onMessage :: Config -> TMVar State -> A.Channel -> (A.Message, A.Envelope) -> IO ()
onMessage cfg statVar chan (m, e) = do
  case decode $ A.msgBody m :: Maybe Input of
    Nothing -> do
      putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
      A.ackEnv e
    Just msg -> do
      putStrLn $ "Valid message " ++ show msg
      forkIO $ do
        replyBodies <- reply cfg msg
        putStrLn $ "got replies: " ++ concatMap show replyBodies
        sequence_ $ flip fmap replyBodies (\b -> publishMessage chan (Output (in_from msg) b "privmsg"))
      A.ackEnv e


data State = State
           { commands :: M.Map String String
           , karma    :: M.Map String Int
           }
           deriving (Show, Read)

initialState = State { commands = M.empty
                     , karma = M.empty }


parseNixpkgs :: String -> [String]
parseNixpkgs s = fmap (\xs -> xs !! 1) matches where
  matches :: [[String]]
  matches = s =~ ("nixpkgs/([^[:space:]]+)+" :: String)

getNixpkgs :: String -> IO (Maybe String)
getNixpkgs s = do
  putStrLn $ "Trying to get contents for " ++ s
  contents <- R.contentsFor "nixOS" "nixpkgs" (Text.pack s) (Just "heads/master")
  case contents of
    Left error -> do
      print error
      return Nothing
    Right contents ->
      return $ Just $ "https://github.com/nixOS/nixpkgs/tree/master/" ++ s

nixpkgs :: String -> IO [String]
nixpkgs s = fmap catMaybes . mapM getNixpkgs $ parseNixpkgs s

nixpkgsPlugin :: MonadIO m => MyPlugin () m
nixpkgsPlugin = MyPlugin () trans "nixpkgs"
  where
    trans (nick, msg) = liftIO $ nixpkgs msg

prToInfo :: (String, Int) -> IO (Maybe String)
prToInfo (p, n) = do
  putStrLn $ "Sending request to github for nixOS/" ++ p ++ "#" ++ show n

  req <- fmap (H.setRequestHeader "user-agent" ["haskell"]) $ H.parseRequest $ "https://api.github.com/repos/nixOS/" ++ p ++ "/issues/" ++ show n
  response <- H.httpJSON req :: IO (H.Response Issue)
  let body = H.getResponseBody response

  return $ Just $ i_url body ++ " (by " ++ i_author body ++ ", " ++ i_state body ++ "): " ++ i_title body


type Plugin = State -> (String, String) -> IO (State, [String])
type NewPlugin = (String, String) -> State

parsePRs :: String -> String -> [(String, Int)]
parsePRs def str = parsed
  where
    matches :: [[String]]
    matches = str =~ ("([[:alpha:]]+)?#([[:digit:]]+)" :: String)
    parsed :: [(String, Int)]
    parsed = map (\(a, b) -> (if null a then def else a, b)) . map (\xs -> (xs !! 1, read $ xs !! 2)) $ matches

prPlugin :: MonadIO m => MyPlugin () m
prPlugin = MyPlugin () trans "pr"
  where
    trans (nick, msg) = liftIO $ fmap catMaybes . mapM prToInfo $ parsePRs "nixpkgs" msg

helloPlugin :: Monad m => MyPlugin () m
helloPlugin = MyPlugin () trans "hello"
  where
    trans (nick, "hello!") = return [ "Hello, " ++ nick ++ "!" ]
    trans _                = return []

data LookupResult = Empty | Exact String | Guess String String

replyLookup :: String -> Maybe String -> LookupResult -> [String]
replyLookup _ _ Empty = []
replyLookup _ Nothing (Exact str) = [str]
replyLookup _ (Just arg) (Exact str) = [arg ++ ": " ++ str]
replyLookup nick Nothing (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", str]
replyLookup nick (Just arg) (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", arg ++ ": " ++ str]

lookupCommand :: String -> M.Map String String -> LookupResult
lookupCommand str map = result
  where
    filtered =
      filter ((3 >=) . snd)
      . sortBy (comparing snd)
      . fmap (\s -> (s,
                     levenshteinDistance defaultEditCosts str s)
             )
      . filter (\s -> (s == str) || ((>=3) . length $ s))
      $ if length str <= 3 then (if M.member str map then [str] else []) else M.keys map
    result = case filtered of
               []         -> Empty
               (str, 0):_ -> Exact . fromJust $ M.lookup str map
               (str, _):_ -> Guess str . fromJust $ M.lookup str map

karmaRegex :: String
karmaRegex = "\\`[[:space:]]*([^[:space:]]+)[[:space:]]*\\+\\+\\'"

karmaPlugin :: Monad m => MyPlugin (M.Map String Int) m
karmaPlugin = MyPlugin M.empty trans "karma"
  where
    trans (nick, msg) =
      case matches of
        Nothing   -> return []
        Just user -> do
          let decrease = user == nick
          let mod = if decrease then (\x -> x - 1) else (\x -> x + 1)
          oldMap <- get
          let newKarma = mod . M.findWithDefault 0 user $ oldMap
          modify (M.insert user newKarma)
          return [ user ++ "'s karma got " ++
            (if decrease then "decreased" else "increased")
            ++ " to " ++ show newKarma ]
      where
        matches = listToMaybe $ fmap (!!1) (msg =~ karmaRegex :: [[String]])

commandsPlugin :: Monad m => MyPlugin (M.Map String String) m
commandsPlugin = MyPlugin M.empty trans "commands"
  where
    trans (nick, ',':command) = case words command of
      [] -> do
        keys <- gets M.keys
        return ["All commands: " ++ unwords keys]
      [ cmd ] -> do
        result <- gets (lookupCommand cmd)
        return $ replyLookup nick Nothing result
      cmd:"=":rest -> case length rest of
          0 -> do
            modify (M.delete cmd)
            return [ cmd ++ " undefined" ]
          _ -> do
            modify (M.insert cmd (unwords rest))
            return [ cmd ++ " defined" ]
      cmd:args -> do
        result <- gets (lookupCommand cmd)
        return $ replyLookup nick (Just (unwords args)) result
    trans (_, _) = return []


-- Each domain has its own state, which is not shared with other domains

type PluginInput = (String, String)
data MyPlugin s m = MyPlugin { initState :: s
                             , transf :: PluginInput -> ST.StateT s m [String]
                             , name :: String
                             }

data Backend s m = Backend { load :: m (Maybe s), store :: s -> m () }

fileBackend :: (Read s, MonadLogger m, Show s, MonadIO m, MonadReader Config m) => FilePath -> Backend s m
fileBackend path = Backend
  { load = do
      statePath <- reader stateDir
      let fullPath = statePath </> path

      liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)
      fileExists <- liftIO $ doesFileExist fullPath
      if not fileExists
        then return Nothing
        else do
          contents <- liftIO $ S.readFile fullPath
          return $ readMaybe contents
  , store = \s -> do
      statePath <- reader stateDir
      let fullPath = statePath </> path
      liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)
      liftIO . writeFile fullPath $ show s
  }

examplePlugin :: Monad m => MyPlugin Int m
examplePlugin = MyPlugin 0 trans "example"
  where
    trans input = do
      value <- get
      put $ value + 1
      return [ "increased value by 1" ]

runPlugin :: MonadLogger m => MyPlugin s m -> Backend s m -> PluginInput -> m [String]
runPlugin (MyPlugin init trans name) (Backend load store) input = do
  state <- fromMaybe init <$> load
  (results, newState) <- ST.runStateT (trans input) state
  store newState
  return results

type RunnablePlugin m = PluginInput -> m [String]

onDomain :: (MonadLogger m, MonadIO m, MonadReader Config m, Read s, Show s) => MyPlugin s m -> String -> RunnablePlugin m
onDomain plugin domain = runPlugin plugin (fileBackend (domain ++ "/" ++ (name plugin)))

newPlugins :: (MonadLogger m, MonadReader Config m, MonadIO m) => String -> [ PluginInput -> m [String] ]
newPlugins "#nixos" = [ karmaPlugin `onDomain` nixOS
                      , prPlugin `onDomain` nixOS
                      , commandsPlugin `onDomain` nixOS
                      ]
newPlugins "#bottest" = [ karmaPlugin `onDomain` nixOS
                        , prPlugin `onDomain` nixOS
                        , helloPlugin `onDomain` nixOS
                        , commandsPlugin `onDomain` nixOS
                        , nixpkgsPlugin `onDomain` nixOS
                        ]
newPlugins "#nixos-borg" = [ karmaPlugin `onDomain` nixOS
                           , prPlugin `onDomain` nixOS
                           , helloPlugin `onDomain` nixOS
                           , commandsPlugin `onDomain` nixOS
                           ]
newPlugins ('#':_) = []
newPlugins nick = [ commandsPlugin `onDomain` nick
                  , helloPlugin `onDomain` nick
                  , karmaPlugin `onDomain` nick
                  ]

-- Domains
nixOS = "nixOS"
testing = "Testing"
global = "Global"


reply :: Config -> Input -> IO [String]
reply cfg Input { in_from = channel, in_sender = nick, in_body = msg } = do
  let chanPlugs = newPlugins channel
  replies <- mapM (\p -> flip runReaderT cfg . runStdoutLoggingT $ p (nick, msg)) chanPlugs
  return $ take 3 $ concat replies
