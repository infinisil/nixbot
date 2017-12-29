{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.List           as L
import           Data.Maybe
import           Control.Exception               (SomeException, handle)
import           Data.Text
import           GHC.Generics
import qualified Network.AMQP        as A
import qualified Network.AMQP.Worker as W
import           System.Environment
import           System.IO

data Input = Input
  { in_from   :: String
  , in_body   :: String
  , in_sender :: String
  } deriving (Show, Generic)

data Output = Output
  { out_target :: String
  , out_body   :: String
  } deriving (Show, Generic)


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
      case reply msg of
        Nothing -> do
          putStrLn "No reply for this body"
          A.ackEnv e
        Just replyBody -> do
          putStrLn $ "Reply for this body is " ++ replyBody
          forkIO $ publishMessage chan
            (Output
              { out_target = in_from msg
              , out_body = replyBody
              }) >> return ()
          A.ackEnv e


reply :: Input -> Maybe String
reply (Input { in_from = "#bottest", in_sender = user, in_body = "hello!" }) =
  Just $ "Hello, " ++ user
reply _           = Nothing
