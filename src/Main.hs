{-# LANGUAGE DeriveGeneric     #-}
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
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text
import           GHC.Generics
import           GitHub.Data.Name
import qualified GitHub.Endpoints.Repos.Contents     as R
import qualified Network.AMQP               as A
import qualified Network.HTTP.Simple        as H
import           System.Environment
import           System.IO
import           Text.Regex.TDFA

data Input = Input
  { in_from   :: String
  , in_body   :: String
  , in_sender :: String
  } deriving (Show, Generic)

data Output = Output
  { out_target :: String
  , out_body   :: String
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
      Object o -> do
        login <- o .: "login"
        return $ Issue title state url login
      _ -> mzero

  parseJSON _ = mzero

instance FromJSON Input where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> fromMaybe s (L.stripPrefix "in_" s) }
instance ToJSON Output where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = \s -> fromMaybe s (L.stripPrefix "out_" s) }

exchange :: A.ExchangeOpts
exchange = A.newExchange
  { A.exchangeName = "exchange-messages"
  , A.exchangeType = "fanout"
  , A.exchangePassive = True
  , A.exchangeDurable = True
  , A.exchangeAutoDelete = False
  }

--input :: W.Queue W.Direct Message
--input = W.queue exchange ""


opts :: A.SASLMechanism -> A.ConnectionOpts
opts auth = A.defaultConnectionOpts
  { A.coVHost = "ircbot"
  , A.coTLSSettings = Just A.TLSTrusted
  , A.coServers = [("events.nix.gsc.io", 5671)]
  , A.coAuth = [auth]
  }


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  cmdArgs <- getArgs
  putStrLn $ "Command line arguments are " ++ show cmdArgs
  args <- if L.length cmdArgs >= 2 then
    return cmdArgs else
    fmap L.words $ readFile "./auth"

  let auth = A.amqplain (pack $ args !! 0) (pack $ args !! 1)
  putStrLn $ "Using authentication: " ++ show args
  server auth

writeDone :: TMVar () -> IO ()
writeDone var = atomically $ putTMVar var ()

server :: A.SASLMechanism -> IO ()
server auth = do

    conn <- A.openConnection'' (opts auth)
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
      tag <- A.consumeMsgs chan myQueue A.Ack (onMessage statVar chan)
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

onMessage :: TMVar State -> A.Channel -> (A.Message, A.Envelope) -> IO ()
onMessage statVar chan (m, e) = do
  case decode $ A.msgBody m :: Maybe Input of
    Nothing -> do
      putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
      A.ackEnv e
    Just msg -> do
      putStrLn $ "Valid message " ++ show msg
      forkIO $ do
        state <- atomically $ takeTMVar statVar
        (newState, replyBodies) <- reply state msg
        atomically $ putTMVar statVar newState
        sequence_ $ flip fmap replyBodies (\b -> publishMessage chan (Output (in_from msg) b))
      A.ackEnv e


data State = State
           { commands :: M.Map String String }
           deriving Show

initialState = State { commands = M.empty }


parseNixpkgs :: String -> [String]
parseNixpkgs s = fmap (\xs -> xs !! 1) matches where
  matches :: [[String]]
  matches = s =~ ("nixpkgs/([^[:space:]]+)+" :: String)

getNixpkgs :: String -> IO (Maybe String)
getNixpkgs s = do
  putStrLn $ "Trying to get contents for " ++ s
  contents <- R.contentsFor "NixOS" "nixpkgs" (pack s) (Just "heads/master")
  case contents of
    Left error -> do
      putStrLn $ show error
      return Nothing
    Right contents -> do
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/master/" ++ s

nixpkgs :: String -> IO [String]
nixpkgs s = fmap catMaybes . mapM getNixpkgs $ parseNixpkgs s

prToInfo :: (String, Int) -> IO (Maybe String)
prToInfo (p, n) = do
  putStrLn $ "Sending request to github for NixOS/" ++ p ++ "#" ++ show n

  req <- fmap (H.setRequestHeader "user-agent" ["haskell"]) $ H.parseRequest $ "https://api.github.com/repos/NixOS/" ++ p ++ "/issues/" ++ show n
  response <- H.httpJSON req :: IO (H.Response Issue)
  let body = H.getResponseBody response

  return $ Just $ i_url body ++ " (by " ++ i_author body ++ ", " ++ i_state body ++ "): " ++ i_title body


type Plugin = State -> (String, String) -> IO (State, [String])

prPlugin :: Plugin
prPlugin state (nick, msg) = do
  results <- fmap catMaybes . mapM prToInfo $ parsed
  return (state, results)
  where
    matches :: [[String]]
    matches = msg =~ ("([[:alpha:]]+)#([[:digit:]]+)" :: String)
    parsed :: [(String, Int)]
    parsed = fmap (\xs -> (xs !! 1, read $ xs !! 2)) matches

helloPlugin :: Plugin
helloPlugin state (nick, "hello!") = return (state, ["Hello, " ++ nick ++ "!"])
helloPlugin state (nick, msg) = return (state, [])

commandsPlugin :: Plugin
commandsPlugin state (_, '!':command) = case L.words command of
  [] -> return (state, [])
  [ cmd ] -> case M.lookup cmd (commands state) of
    Nothing -> return (state, ["Invalid command"])
    Just result -> return (state, [result])
  cmd:"=":rest ->
    return
      ( state { commands = M.insert cmd (L.unwords rest) (commands state) }
      , ["Set command " ++ cmd ++ " to " ++ L.unwords rest] )
  _ -> return (state, [])
commandsPlugin state (_, _) = return (state, [])

plugins :: String -> [Plugin]
plugins "#nixos" = [ prPlugin ]
plugins "#bottest" = [ prPlugin, helloPlugin, commandsPlugin ]
plugins _ = []

reply :: State -> Input -> IO (State, [String])
reply state Input { in_from = channel, in_sender = nick, in_body = msg } = do
  let chanPlugs = plugins channel
  (finalState, replies) <- foldM (
    \(state, replies) plugin -> do
      (newState, additionalReplies) <- plugin state (nick, msg)
      return (newState, replies ++ additionalReplies)
    ) (state, []) chanPlugs
    
  return (finalState, replies)
