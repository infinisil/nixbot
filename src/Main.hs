{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
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
import qualified Network.AMQP               as A
import qualified Network.AMQP.Worker        as W
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

data PR = PR
  { pr_title  :: String
  , pr_state  :: String
  , pr_url    :: String
  , pr_author :: String
  } deriving (Show, Generic)

instance FromJSON PR where
  parseJSON (Object v) = do
    title <- v .: "title"
    state <- v .: "state"
    url <- v .: "html_url"
    user <- v .: "user"
    case user of
      Object o -> do
        login <- o .: "login"
        return $ PR title state url login
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
    args <- fmap L.words $ readFile "./auth"
    let auth = A.amqplain (pack $ args !! 0) (pack $ args !! 1)
    server auth

server :: A.SASLMechanism -> IO ()
server auth = do
    conn <- A.openConnection'' (opts auth)
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

      tag <- A.consumeMsgs chan myQueue A.Ack (onMessage chan)
      putStrLn $ "Started consumer with tag " ++ show tag

      putStrLn $ "terminating if enter is pressed..."
      getLine



      res <- A.waitForConfirms chan
      putStrLn $ "waited for confirms with result: " ++ show res
      A.closeConnection conn
      putStrLn "connection closed"

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

onMessage :: A.Channel -> (A.Message, A.Envelope) -> IO ()
onMessage chan (m, e) = do
  case decode $ A.msgBody m :: Maybe Input of
    Nothing -> do
      putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
      A.ackEnv e
    Just msg -> do
      putStrLn $ "Valid message " ++ show msg
      forkIO $ do
        replyBodies <- reply msg
        sequence_ $ flip fmap replyBodies (\b -> publishMessage chan (Output (in_from msg) b))
      A.ackEnv e


data State = State
           { commands :: M.Map String String }
           deriving Show

parse :: String -> [(String, Int)]
parse s = fmap (\xs -> (xs !! 1, read $ xs !! 2)) matches where
  matches :: [[String]]
  matches = s =~ ("([[:alpha:]]+)#([[:digit:]]+)" :: String)

prToInfo :: (String, Int) -> IO (Maybe String)
prToInfo (p, n) = do
  putStrLn $ "Sending request to github for NixOS/" ++ p ++ "#" ++ show n

  req <- fmap (H.setRequestHeader "user-agent" ["haskell"]) $ H.parseRequest $ "https://api.github.com/repos/NixOS/" ++ p ++ "/pulls/" ++ show n
  response <- H.httpJSON req :: IO (H.Response PR)
  let body = H.getResponseBody response

  return $ Just $ pr_url body ++ " (by " ++ pr_author body ++ ", " ++ pr_state body ++ "): " ++ pr_title body


reply :: Input -> IO [String]
reply (Input { in_from = "#bottest", in_sender = user, in_body = "hello!" }) = return ["Hello, " ++ user]
reply (Input { in_body = body }) = fmap catMaybes . mapM prToInfo $ parse body
reply _           = return []
