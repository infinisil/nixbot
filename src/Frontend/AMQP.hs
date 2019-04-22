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


setup :: Env -> IO (A.Connection, A.Channel, TMVar SomeException)
setup env@Env { config, frontend } = do
  waiter <- newEmptyTMVarIO

  conn <- A.openConnection'' (amqpOptions config)
  logMsgEnv env "AMQP connection opened"
  A.addConnectionClosedHandler conn True $
    logMsgEnv env "AMQP connection closed"

  chan <- A.openChannel conn
  atomically $ putTMVar (amqpChannel frontend) chan
  logMsgEnv env "AMQP channel opened"
  A.addChannelExceptionHandler chan $ \exc -> case fromException exc of
    Just (A.ConnectionClosedException A.Normal _) -> logMsgEnv env "AMQP channel closed"
    _ -> do
      logMsgEnv env $ "AMQP channel exception: " <> show' exc
      atomically $ putTMVar waiter exc

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

action :: Env -> (Input -> App ()) -> (A.Connection, A.Channel, TMVar SomeException) -> IO a
action env onInput (_, chan, waiter) = do

  (queue, _, _) <- A.declareQueue chan queueOpts
  liftIO $ A.declareExchange chan exchangeOpts
  A.bindQueue chan queue (A.exchangeName exchangeOpts) "queue-publish"

  _ <- A.consumeMsgs chan queue A.Ack onMsg
  logMsgEnv env "AMQP consumer started"

  exc <- atomically $ takeTMVar waiter
  throwIO exc
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

tearDown :: Env -> (A.Connection, A.Channel, TMVar SomeException) -> IO ()
tearDown env (conn, _, _) = do
  logMsgEnv env "Teardown called, closing the connection"
  A.closeConnection conn

loop :: Env -> (Input -> App ()) -> IO ()
loop env onInput = do
  shouldRestart <- catch oneConnection handleException
  when shouldRestart $ loop env onInput
  where
    oneConnection = bracket
      (setup env)
      (tearDown env)
      (action env onInput)

    handleException exc = case fromException exc of
      Just UserInterrupt -> do
        logMsgEnv env "User interrupt, stopping.."
        return False
      _                  -> do
        logMsgEnv env $ "Exception: " <> show' exc
        logMsgEnv env "Restarting.."
        return True
