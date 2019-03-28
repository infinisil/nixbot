{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}


module Main where

import           Config
import           IRC
import           Plugins
import           Plugins.Commands
import           Plugins.Commands.Tell
import           Plugins.Karma
import           Plugins.Leaked
import           Plugins.NixRepl
import           Plugins.Pr
import           Plugins.Unreg
import           Types

import           Control.Concurrent     (forkIO, myThreadId)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types       (fieldLabelModifier)
import           Data.List              (stripPrefix)
import           Data.Maybe
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Data.Text              (pack)
import           GHC.Generics           (Generic)
import qualified Network.AMQP           as A
import           System.Directory
import           System.FilePath
import           System.IO              (BufferMode (..), hSetBuffering, stdout)
import           System.Posix.Signals

data RInput = RInput
  { in_from   :: String
  , in_body   :: String
  , in_sender :: String
  } deriving (Show, Generic)

toPluginInput :: RInput -> Input
toPluginInput RInput { in_from, in_body, in_sender } = Input
  { inputUser = in_sender
  , inputMessage = in_body
  , inputChannel = case in_from of
      '#':channel -> Just channel
      _           -> Nothing
  }

data Output = Output
  { out_target       :: String
  , out_body         :: String
  , out_message_type :: String
  } deriving (Show, Generic)

instance FromJSON RInput where
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


main :: IO ()
main = bracket setup tearDown start where
  setup :: IO (Env, TMVar ())
  setup = do
    hSetBuffering stdout LineBuffering
    mainThread <- myThreadId
    _ <- installHandler sigTERM (Catch (throwTo mainThread UserInterrupt)) Nothing

    config <- getConfig
    conn <- A.openConnection'' (amqpOptions config)
    chan <- A.openChannel conn

    let sharedStateFile = stateDir config </> "new/shared"
    exists <- doesFileExist sharedStateFile
    sharedState <- if not exists then return (SharedState Set.empty) else
      decodeFileStrict sharedStateFile >>= \case
        Nothing -> do
          putStrLn "Error decoding shared state file"
          return $ SharedState Set.empty
        Just result -> return result
    sharedStateVar <- newTVarIO sharedState

    var <- liftIO newEmptyTMVarIO
    A.addConnectionClosedHandler conn True $ atomically $ putTMVar var ()

    return (Env config chan sharedStateVar, var)

  tearDown :: (Env, TMVar ()) -> IO ()
  tearDown (env, _) = do
    let sharedStateFile = stateDir (config env) </> "new/shared"
    putStrLn "Shutting down, saving global state"
    finalSharedState <- readTVarIO (sharedState env)
    encodeFile sharedStateFile finalSharedState


start :: (Env, TMVar ()) -> IO ()
start (env, var) = flip runReaderT env $ runStderrLoggingT $ do
  chan <- reader amqpChannel

  (publishQueueName, _, _) <- liftIO $ A.declareQueue chan $ A.newQueue
    { A.queueName = "queue-publish"
    , A.queuePassive = True
    }
  logDebugN $ "Declared queue with name " <> pack (show publishQueueName)

  (myQueue, _, _) <- liftIO $ A.declareQueue chan $ A.newQueue
    { A.queueName = ""
    , A.queueAutoDelete = True
    , A.queueExclusive = True
    }
  logDebugN $ "Declared my queue with name " <> pack (show myQueue)

  liftIO $ A.declareExchange chan exchange
  logDebugN "Declared exchange"

  liftIO $ A.bindQueue chan myQueue "exchange-messages" "queue-publish"
  logDebugN "Bound queue"

  liftIO $ A.confirmSelect chan False
  logDebugN "disabled nowait"

  r <- ask
  tag <- liftIO $ A.consumeMsgs chan myQueue A.Ack (\x -> do
                                                       _ <- forkIO (runReaderT (onMessage x) r)
                                                       return ()
                                                   )
  logDebugN $ "Started consumer with tag " <> pack (show tag)

  liftIO $ atomically $ takeTMVar var

publishMessage :: A.Channel -> Output -> IO (Maybe Int)
publishMessage chan msg = do
  putStrLn $ "Sending the message " <> show msg
  -- publish a message to our new exchange
  intMb <- A.publishMsg chan "" "queue-publish" A.newMsg
    { A.msgBody = encode msg
    , A.msgDeliveryMode = Just A.Persistent}

  putStrLn $ "Published Message" <> maybe "" (\s -> ", got sequence number " ++ show s) intMb
  return intMb

newtype IRCT m a = IRCT { runIRCT :: A.Channel -> TVar SharedState -> m a } deriving (Functor)

instance Applicative m => Applicative (IRCT m) where
  pure a = IRCT $ \_ _ -> pure a
  IRCT a <*> IRCT b = IRCT $ \chan st -> let af = a chan st; bf = b chan st in af <*> bf

instance Monad m => Monad (IRCT m) where
  IRCT a >>= f = IRCT $ \chan st -> let
    af = a chan st
    in af >>= (\y -> runIRCT (f y) chan st)

instance MonadIO m => MonadIO (IRCT m) where
  liftIO action = IRCT $ \_ _ -> liftIO action

instance MonadTrans IRCT where
  lift a = IRCT $ \_ _ -> a

instance MonadLogger m => MonadLogger (IRCT m) where
instance MonadReader r m => MonadReader r (IRCT m) where
  ask = lift ask
  local f a = IRCT $ \chan st -> local f $ runIRCT a chan st
  reader = lift . reader

instance MonadIO m => IRCMonad (IRCT m) where
  privMsg user message = IRCT $ \chan _ -> do
    let output = encode $ Output user message "privmsg"
    _ <- liftIO $ A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = output
      , A.msgDeliveryMode = Just A.Persistent
      }
    return ()
  chanMsg channel message = IRCT $ \chan _ -> do
    let output = encode $ Output ('#':channel) message "privmsg"
    _ <- liftIO $ A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = output
      , A.msgDeliveryMode = Just A.Persistent
      }
    return ()
  isKnown user = IRCT $ \_ st ->
    liftIO $ atomically $ Set.member user . knownUsers <$> readTVar st

traceUser :: Input -> ReaderT Env IO ()
traceUser Input { inputUser } = do
  var <- asks sharedState
  new <- liftIO $ atomically $ do
    s <- readTVar var
    writeTVar var $ s { knownUsers = Set.insert inputUser (knownUsers s) }
    return $ not $ Set.member inputUser $ knownUsers s
  when new $ liftIO $ putStrLn $ "Recorded new user: " ++ inputUser

onMessage :: (A.Message, A.Envelope) -> ReaderT Env IO ()
onMessage (m, e) = runStderrLoggingT $ do
  case decode $ A.msgBody m :: Maybe RInput of
    Nothing -> liftIO $ putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
    Just msg -> do
      let input = toPluginInput msg
      lift $ traceUser input
      env <- ask
      ps <- plugins (inputChannel input)
      _ <- runIRCT (runPlugins ps input) (amqpChannel env) (sharedState env)
      return ()

  liftIO $ A.ackEnv e

prSettings :: Settings
prSettings = Settings
  { defOwner = \case
      "home-manager" -> "rycee"
      "cachix" -> "cachix"
      "nix-darwin" -> "LnL7"
      "NUR" -> "nix-community"
      "wiki" -> "nix-community"
      "hnix" -> "haskell-nix"
      _ -> "NixOS"
  , defRepo = "nixpkgs"
  , prFilter = \case
      ParsedIssue Hash "NixOS" "nixpkgs" number -> number >= 10
      _ -> True
  }


examplePlugin :: Plugin
examplePlugin = Plugin
  { pluginName = "example"
  , pluginCatcher = Catched True
  , pluginHandler = \Input { inputChannel, inputUser } ->
      case (inputChannel, inputUser) of
        (Nothing, "infinisil") -> privMsg inputUser "I have received your message"
        (Just "bottest", _) -> chanMsg "bottest" $ inputUser ++ ": I have received your message"
        _ -> return ()
  }

developFilter :: Plugin
developFilter = Plugin
  { pluginName = "develop-filter"
  , pluginCatcher = \Input { inputChannel, inputUser } ->
      if inputChannel == Just "bottest" || inputChannel == Nothing && inputUser == "infinisil"
      then PassedOn else Catched True ()
  , pluginHandler = const (return ())
  }

plugins :: MonadReader Env m => Maybe Channel -> m [Plugin]
-- TODO: Add a better way to configure channels
plugins (Just "pijul") = return
  [ tellSnooper
  , karmaPlugin
  ]
plugins _ = do
  debug <- asks (debugMode . config)

  return $ [ developFilter | debug ]
    ++
    [ leakedPlugin
    , unregPlugin
    , tellSnooper
    , commandsPlugin'
    , nixreplPlugin
    , karmaPlugin
    , prPlugin prSettings
    ]


