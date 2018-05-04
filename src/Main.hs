{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception          (SomeException, handle)
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types           (fieldLabelModifier)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Text.Read (readMaybe)
import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import           GHC.Generics
import           GitHub.Data.Name
import qualified GitHub.Endpoints.Repos.Contents     as R
import qualified Network.AMQP               as A
import qualified Network.HTTP.Simple        as H
import           System.Environment
import           System.IO
import qualified System.IO.Strict as S
import           Text.Regex.TDFA
import Text.EditDistance
import Data.Ord (comparing)

data Input = Input
  { in_from   :: String
  , in_body   :: String
  , in_sender :: String
  } deriving (Show, Generic)

data Output = Output
  { out_target :: String
  , out_body   :: String
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
      _ -> mzero

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
  
opts :: A.SASLMechanism -> A.ConnectionOpts
opts auth = A.defaultConnectionOpts
  { A.coVHost = "ircbot"
  , A.coTLSSettings = Just A.TLSTrusted
  , A.coServers = [("events.nix.gsc.io", 5671)]
  , A.coAuth = [auth]
  }

main = do
  hSetBuffering stdout LineBuffering
  cmdArgs <- getArgs
  let sp = cmdArgs !! 0
  let rest = tail cmdArgs
  putStrLn $ "Command line arguments are " ++ show cmdArgs
  args <- if length rest >= 2 then
    return rest else
    fmap words $ readFile "./auth"

  let config = Config { auth = A.amqplain (Text.pack $ args !! 0) (Text.pack $ args !! 1)
                      , statePath = sp }
  putStrLn $ "Using statePath: " ++ statePath config
  server config

writeDone :: TMVar () -> IO ()
writeDone var = atomically $ putTMVar var ()

data Config = Config
  { auth :: A.SASLMechanism
  , statePath :: String
  }
  

server :: Config -> IO ()
server cfg = do
  
    conn <- A.openConnection'' (opts (auth cfg))
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
        state <- fmap (fromMaybe initialState . readMaybe) . S.readFile $ (statePath cfg)
        (newState, replyBodies) <- reply state msg
        putStrLn $ "got replies: " ++ concatMap show replyBodies
        writeFile (statePath cfg) (show newState)
        sequence_ $ flip fmap replyBodies (\b -> publishMessage chan (Output (in_from msg) b "privmsg"))
      A.ackEnv e


data State = State
           { commands :: M.Map String String
           , karma :: M.Map String Int
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
  contents <- R.contentsFor "NixOS" "nixpkgs" (Text.pack s) (Just "heads/master")
  case contents of
    Left error -> do
      print error
      return Nothing
    Right contents -> 
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/master/" ++ s

nixpkgs :: String -> IO [String]
nixpkgs s = fmap catMaybes . mapM getNixpkgs $ parseNixpkgs s

nixpkgsPlugin :: Plugin
nixpkgsPlugin state (nick, msg) = do
  putStrLn $ "Trying to get nixpkgs links for message " ++ msg 
  links <- nixpkgs msg
  return (state, links)

prToInfo :: (String, Int) -> IO (Maybe String)
prToInfo (p, n) = do
  putStrLn $ "Sending request to github for NixOS/" ++ p ++ "#" ++ show n

  req <- fmap (H.setRequestHeader "user-agent" ["haskell"]) $ H.parseRequest $ "https://api.github.com/repos/NixOS/" ++ p ++ "/issues/" ++ show n
  response <- H.httpJSON req :: IO (H.Response Issue)
  let body = H.getResponseBody response

  return $ Just $ i_url body ++ " (by " ++ i_author body ++ ", " ++ i_state body ++ "): " ++ i_title body


type Plugin = State -> (String, String) -> IO (State, [String])

parsePRs :: String -> String -> [(String, Int)]
parsePRs def str = parsed
  where
    matches :: [[String]]
    matches = str =~ ("([[:alpha:]]+)?#([[:digit:]]+)" :: String)
    parsed :: [(String, Int)]
    parsed = map (\(a, b) -> (if null a then def else a, b)) . map (\xs -> (xs !! 1, read $ xs !! 2)) $ matches

prPlugin :: Plugin
prPlugin state (nick, msg) = do
  results <- fmap catMaybes . mapM prToInfo $ parsePRs "nixpkgs" msg
  return (state, results)

helloPlugin :: Plugin
helloPlugin state (nick, "hello!") = return (state, ["Hello, " ++ nick ++ "!"])
helloPlugin state (nick, msg) = return (state, [])

data LookupResult = Empty | Exact String | Guess String String

replyLookup :: String -> Maybe String -> LookupResult -> [String]
replyLookup _ _ Empty = []
replyLookup _ Nothing (Exact str) = [str]
replyLookup _ (Just arg) (Exact str) = [arg ++ ": " ++ str]
replyLookup nick Nothing (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", str]
replyLookup nick (Just arg) (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", arg ++ ": " ++ str]

lookupCommand :: M.Map String String -> String -> LookupResult
lookupCommand map str = result
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
               [] -> Empty
               (str, 0):_ -> Exact . fromJust $ M.lookup str map
               (str, _):_ -> Guess str . fromJust $ M.lookup str map

karmaRegex :: String
karmaRegex = "\\`[[:space:]]*([^[:space:]]+)[[:space:]]*\\+\\+\\'"

karmaPlugin :: Plugin
karmaPlugin state (nick, msg) = 
      case matches of
        Nothing -> return (state, [])
        Just user -> return $ adjust user (user == nick)
      where
        matches = listToMaybe $ fmap (!!1) (msg =~ karmaRegex :: [[String]])
        adjust :: String -> Bool -> (State, [String])
        adjust user decrease = (state { karma = newKarmaMap }, [message])
          where
            mod = if decrease then (\x -> x - 1) else (\x -> x + 1)
            newKarma = mod $ M.findWithDefault 0 user (karma state)
            newKarmaMap = M.insert user newKarma (karma state)
            message = user ++ "'s karma got " ++
              (if decrease then "decreased" else "increased")
              ++ " to " ++ show newKarma



commandsPlugin :: Plugin
commandsPlugin state (nick, ',':command) = case words command of
  [] -> return (state, ["All commands: " ++ unwords (M.keys (commands state))])
  [ cmd ] -> return (state, replyLookup nick Nothing (lookupCommand (commands state) cmd))
  cmd:"=":rest -> case length rest of
    0 -> return
      ( state { commands = M.delete cmd (commands state) }
      , [cmd ++ " undefined"] )
    _ -> return
      ( state { commands = M.insert cmd (unwords rest) (commands state) }
      , [cmd ++ " defined"] )
  cmd:args -> return (state, replyLookup nick (Just (unwords args)) (lookupCommand (commands state) cmd))
commandsPlugin state (_, _) = return (state, [])

plugins :: String -> [Plugin]
plugins "#nixos" = [ karmaPlugin, prPlugin, commandsPlugin ]
plugins "#bottest" = [ karmaPlugin, prPlugin, helloPlugin, commandsPlugin, nixpkgsPlugin ]
plugins "#nixos-borg" = [ karmaPlugin, prPlugin, helloPlugin, commandsPlugin ]
plugins _ = []

reply :: State -> Input -> IO (State, [String])
reply state Input { in_from = channel, in_sender = nick, in_body = msg } = do
  let chanPlugs = plugins channel
  (finalState, replies) <- foldM (
    \(state, replies) plugin -> do
      (newState, additionalReplies) <- plugin state (nick, msg)
      return (newState, replies ++ additionalReplies)
    ) (state, []) chanPlugs

  return (finalState, take 3 replies)
