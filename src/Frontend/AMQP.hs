{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           System.Timeout
import           Types

-- | Creates a new frontend with an output queue and a yet-non-existant amqp channel
initFrontend :: IO Frontend
initFrontend = Frontend <$> newTMQueueIO <*> newEmptyTMVarIO

-- | Runs the AMQP frontend with a handler for incoming messages and outgoing messages
runFrontend :: (Input -> App ()) -> App ()
runFrontend onInput = do
  env <- ask
  -- Asynchronously run a send thread
  sendThread <- lift $ async (sender env)

  -- Treat SIGTERM as UserInterrupt, then shutting down is the same for systemd and the terminal
  mainThread <- lift myThreadId
  _ <- lift $ installHandler sigTERM (Catch (throwTo mainThread UserInterrupt)) Nothing

  -- Run the connection loop
  lift $ loop Nothing env onInput

  -- Close the output queue, then wait until all messages have been sent (= send thread returns)
  -- FIXME: If the above loop exited, we won't have a channel anymore, so if we still have messages to send, we wait forever
  -- because the sender thread needs to send all messages before it can exit
  queue <- asks (outputQueue . frontend)
  lift $ atomically $ closeTMQueue queue
  lift $ wait sendThread

sendFrontend :: Output -> App ()
sendFrontend output = do
  queue <- asks (outputQueue . frontend)
  lift $ atomically $ writeTMQueue queue output


setup :: Env -> IO (A.Connection, A.Channel, TMVar SomeException)
setup env@Env { config, frontend } = do
  -- A waiter is there to tell us when the connection closed
  waiter <- newEmptyTMVarIO

  conn <- A.openConnection'' (amqpOptions config)
  logMsgEnv env "AMQP connection opened"
  A.addConnectionClosedHandler conn True $
    logMsgEnv env "AMQP connection closed"

  -- Open a channel and put this into the frontend to signal it having been opened
  chan <- A.openChannel conn
  atomically $ putTMVar (amqpChannel frontend) chan
  logMsgEnv env "AMQP channel opened"
  A.addChannelExceptionHandler chan $ \exc -> do
    -- Immediately remove the channel from the frontend to prevent the sender from sending any more messages to it
    -- Until a new channel has been opened
    _ <- atomically $ takeTMVar (amqpChannel frontend)
    case fromException exc of
      Just (A.ConnectionClosedException A.Normal reason) -> logMsgEnv env $ "AMQP channel closed normally because: " <> Text.pack reason
      _ -> do
        logMsgEnv env $ "AMQP channel closed with exception: " <> show' exc
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

run :: Env -> (Input -> App ()) -> (A.Connection, A.Channel, TMVar SomeException) -> IO a
run env onInput (_, chan, waiter) = do
  logMsgEnv env "Setting up AMQP consumer"

  -- Set up for consuming messages
  (queue, _, _) <- A.declareQueue chan queueOpts
  liftIO $ A.declareExchange chan exchangeOpts
  A.bindQueue chan queue (A.exchangeName exchangeOpts) "queue-publish"

  _ <- A.consumeMsgs chan queue A.Ack onMsg
  logMsgEnv env "AMQP consumer started"

  -- Wait until an exception
  exc <- atomically $ takeTMVar waiter
  throwIO exc
  where
    onMsg :: (A.Message, A.Envelope) -> IO ()
    onMsg (msg, envelope) = void $ forkIO $ flip runReaderT env $
      case decodeInput (A.msgBody msg) of
        Left err -> logMsg $ "Failed to decode " <> toStrict (decodeUtf8 (A.msgBody msg)) <> ": " <> Text.pack err
        Right input -> do
          -- logMsg $ "Received input: " <> show' input
          onInput input
          lift $ A.ackEnv envelope

-- | Waits for messages on outputQueue and sends them to currently open amqpChannel
-- Waits until a channel is open if not.
sender :: Env -> IO ()
sender env@Env { frontend } = do
  -- Get a message to send and the AMQP channel to send it to at the same time
  tosend <- atomically $ readTMQueue (outputQueue frontend) >>= \case
    Nothing -> return Nothing
    Just msg -> do
      chan <- readTMVar (amqpChannel frontend)
      return $ Just (chan, msg)
  case tosend of
    Nothing -> logMsgEnv env "Finished sending, sender queue empty and closed"
    Just (chan, msg) -> do
      -- logMsgEnv env $ "Sending message " <> show' msg
      -- TODO: Make sure the message arrives, put it back into the output queue if it didn't
      _ <- A.publishMsg chan "" "queue-publish" A.newMsg
        { A.msgBody = encodeOutput msg
        , A.msgDeliveryMode = Just A.Persistent
        }
      sender env

decodeInput :: BS.ByteString -> Either String Input
decodeInput bytes = shapeInput <$> eitherDecode' bytes

encodeOutput :: Output -> BS.ByteString
encodeOutput = encode . shapeOutput

tearDown :: Env -> (A.Connection, A.Channel, TMVar SomeException) -> IO ()
tearDown env (conn, _, _) = do
  logMsgEnv env "Teardown called, closing the connection"
  A.closeConnection conn
  logMsgEnv env "Closed the connection"

data FrontendException = Timeout deriving (Eq, Show)
instance Exception FrontendException

data ShouldRestart = NoRestart
                   | RestartWithDelay
                   deriving (Eq, Show)

-- | Try to establish a connection and channel for this many microseconds
connectionTimeout :: Int
connectionTimeout = 5 * 1000 * 1000
-- | When timed out, delay retry by this amount of microseconds initially
minDelay :: Int
minDelay = 100 * 1000
-- | Maximum delay for exponential backoff in microseconds
maxDelay :: Int
maxDelay = 60 * 1000 * 1000

-- | Opens an amqp connection and channel.
-- If an exception occurs or it takes too long to connect, restart with a new connection and channel
-- Only exits on user interrupt
loop :: Maybe Int -> Env -> (Input -> App ()) -> IO ()
loop mdelay env onInput = do
  -- Run one connection until exception. The exception decides whether it should get restarted
  shouldRestart <- catch oneConnection handleException
  logMsgEnv env $ "one connection exited, restart? " <> show' shouldRestart

  case (shouldRestart, mdelay) of
    (NoRestart, _) -> return ()
    (RestartWithDelay, Nothing) -> loop (Just minDelay) env onInput
    (RestartWithDelay, Just delay) -> do
      threadDelay delay
      let newDelay = delay * 2
      -- Because apparently sometimes full restarting is needed,
      -- we do that when our exponential backoff has stepped over the maximum
      when (newDelay < maxDelay) $ loop (Just newDelay) env onInput
  where
    -- Try to set up connection for a certain time, throw exception if timed out
    setup' = do
      result <- timeout connectionTimeout $ setup env
      case result of
        Nothing  -> throwIO Timeout
        Just res -> return res

    oneConnection = bracket
      setup'
      (tearDown env)
      (run env onInput)

    -- Converts an exception into how and whether it should be restart
    handleException exc
      | fromException exc == Just UserInterrupt = do
          logMsgEnv env "User interrupt"
          return NoRestart
      | fromException exc == Just Timeout = do
          logMsgEnv env "Timed out"
          return RestartWithDelay
    handleException exc = do
      logMsgEnv env $ "Exception: " <> show' exc
      return RestartWithDelay
