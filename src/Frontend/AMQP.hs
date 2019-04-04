{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Frontend.AMQP
  ( initFrontend
  , runFrontend
  , sendFrontend
  ) where

import           Config
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Exception
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as BS
import qualified Data.Text                      as Text
import           Data.Text.Lazy                 (toStrict)
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import           Frontend.Types
import           Log
import qualified Network.AMQP                   as A
import           System.Posix.Signals
import           Types

initFrontend :: IO Frontend
initFrontend = Frontend <$> newTMQueueIO <*> newEmptyTMVarIO

runFrontend :: (Input -> App ()) -> App ()
runFrontend onInput = do
  env <- ask
  sendThread <- lift $ async (sender env)

  mainThread <- lift myThreadId
  _ <- lift $ installHandler sigTERM (Catch (throwTo mainThread UserInterrupt)) Nothing
  lift $ loop env onInput
  queue <- asks (outputQueue . frontend)
  lift $ atomically $ closeTMQueue queue
  lift $ wait sendThread

sendFrontend :: Output -> App ()
sendFrontend output = do
  queue <- asks (outputQueue . frontend)
  lift $ atomically $ writeTMQueue queue output


setup :: Env -> IO (A.Connection, A.Channel, TMVar ())
setup env@Env { config, frontend } = do
  waiter <- newEmptyTMVarIO

  conn <- A.openConnection'' (amqpOptions config)
  logMsgEnv env "AMQP connection opened"
  A.addConnectionClosedHandler conn True $ do
    logMsgEnv env "AMQP connection closed"
    atomically $ putTMVar waiter ()

  chan <- A.openChannel conn
  atomically $ putTMVar (amqpChannel frontend) chan
  logMsgEnv env "AMQP channel opened"
  A.addChannelExceptionHandler chan $ \exc -> case fromException exc of
    Just (A.ConnectionClosedException A.Normal _) -> logMsgEnv env "AMQP channel closed"
    _ -> logMsgEnv env $ "AMQP channel exception: " <> show' exc

  return (conn, chan, waiter)

queueOpts :: A.QueueOpts
queueOpts = A.newQueue
  { A.queueName = ""
  , A.queueAutoDelete = False
  , A.queueExclusive = True
  , A.queueDurable = True
  }

exchangeOpts :: A.ExchangeOpts
exchangeOpts = A.newExchange
  { A.exchangeName = "exchange-messages"
  , A.exchangeType = "fanout"
  , A.exchangePassive = True
  , A.exchangeDurable = True
  , A.exchangeAutoDelete = False
  }

action :: Env -> (Input -> App ()) -> (A.Connection, A.Channel, TMVar ()) -> IO ()
action env onInput (_, chan, waiter) = do

  (queue, _, _) <- A.declareQueue chan queueOpts
  liftIO $ A.declareExchange chan exchangeOpts
  A.bindQueue chan queue (A.exchangeName exchangeOpts) "queue-publish"

  _ <- A.consumeMsgs chan queue A.Ack onMsg
  logMsgEnv env "AMQP consumer started"

  atomically $ takeTMVar waiter
  where
    onMsg :: (A.Message, A.Envelope) -> IO ()
    onMsg (msg, envelope) = flip runReaderT env $ do
      decodeInput (A.msgBody msg) >>= maybe (return ()) onInput
      lift $ A.ackEnv envelope

sender :: Env -> IO ()
sender env@Env { frontend } = do
  mmsg <- atomically $ readTMQueue (outputQueue frontend)
  case mmsg of
    Nothing -> logMsgEnv env "Finished sending, sender queue empty and closed"
    Just msg -> do
      chan <- atomically $ readTMVar (amqpChannel frontend)
      _ <- A.publishMsg chan "" "queue-publish" A.newMsg
        { A.msgBody = encodeOutput msg
        , A.msgDeliveryMode = Just A.Persistent
        }
      sender env


decodeInput :: BS.ByteString -> App (Maybe Input)
decodeInput bytes = case eitherDecode' bytes of
  Left err -> do
    logMsg $ "Failed to decode " <> toStrict (decodeUtf8 bytes) <> ": " <> Text.pack err
    return Nothing
  Right (shapeInput -> res) -> return $ Just res

encodeOutput :: Output -> BS.ByteString
encodeOutput = encode . shapeOutput

tearDown :: (A.Connection, A.Channel, TMVar ()) -> IO ()
tearDown (conn, _, _) = A.closeConnection conn

loop :: Env -> (Input -> App ()) -> IO ()
loop env onInput = catch oneConnection $ \exc -> case fromException exc of
  Just UserInterrupt ->
    logMsgEnv env "User interrupt, stopping.."
  _                  -> do
    logMsgEnv env $ "Exception: " <> show' exc
    logMsgEnv env "Restarting.."
    loop env onInput
  where
    oneConnection = bracket
      (setup env)
      tearDown
      (action env onInput)
