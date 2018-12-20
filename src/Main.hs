{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}


module Main where

import           Config
import           IRC
import           Plugins
import           Plugins.Commands
import           Plugins.Karma
import           Plugins.Leaked
import           Plugins.Nixpkgs
import           Plugins.NixRepl
import           Plugins.Pr
import           Plugins.Reply
import           Plugins.Tell
import           Plugins.Unreg
import           Types

import           Control.Concurrent              (forkIO, myThreadId)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Exception.Base          (IOException)
import           Control.Monad                   (foldM, mzero)
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Control.Monad.State             as ST
import           Control.Monad.State.Class
import           Data.Aeson
import           Data.Aeson.Types                (fieldLabelModifier)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8      as BS
import           Data.Data
import           Data.Functor.Identity
import           Data.List                       (sortBy, stripPrefix)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Monoid                     ((<>))
import           Data.Ord                        (comparing)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text, pack, unpack)
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as TIO
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
import           System.Posix.Signals
import           Text.EditDistance               (defaultEditCosts,
                                                  levenshteinDistance)
import           Text.Read                       (readMaybe)
import           Text.Regex.TDFA                 ((=~))


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


main = bracket setup tearDown start where
  setup :: IO (Env, TMVar ())
  setup = do
    hSetBuffering stdout LineBuffering
    mainThread <- myThreadId
    installHandler sigTERM (Catch (throwTo mainThread UserInterrupt)) Nothing

    config <- getConfig
    conn <- A.openConnection'' (amqpOptions config)
    chan <- A.openChannel conn

    let sharedStateFile = stateDir config </> "new/shared"
    exists <- doesFileExist sharedStateFile
    sharedState <- if not exists then return (SharedState Set.empty) else do
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
  tearDown (env, var) = do
    let sharedStateFile = stateDir (config env) </> "new/shared"
    putStrLn "Shutting down, saving global state"
    finalSharedState <- atomically $ readTVar (sharedState env)
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

  cfg <- reader config
  cache <- liftIO $ Text.lines <$> TIO.readFile "pathcache"
  r <- ask
  tag <- liftIO $ A.consumeMsgs chan myQueue A.Ack (\x -> do
                                                       forkIO (runReaderT (onMessage cache x) r)
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

instance MonadIO m => IRCMonad (ReaderT Env m) where
  privMsg user message = ReaderT $ \(Env _ chan _) -> do
    let output = encode $ Output user message "privmsg"
    liftIO $ A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = output
      , A.msgDeliveryMode = Just A.Persistent
      }
    return ()
  chanMsg channel message = ReaderT $ \(Env _ chan _) -> do
    let output = encode $ Output ('#':channel) message "privmsg"
    liftIO $ A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = output
      , A.msgDeliveryMode = Just A.Persistent
      }
    return ()
  isKnown user = ReaderT $ \(Env _ _ sharedState) ->
    liftIO $ atomically $ Set.member user . knownUsers <$> readTVar sharedState

traceUser :: Input -> ReaderT Env IO ()
traceUser Input { inputUser } = do
  var <- asks sharedState
  new <- liftIO $ atomically $ do
    state <- readTVar var
    writeTVar var $ state { knownUsers = Set.insert inputUser (knownUsers state) }
    return $ not $ Set.member inputUser $ knownUsers state
  when new $ liftIO $ putStrLn $ "Recorded new user: " ++ inputUser

onMessage :: [Text] -> (A.Message, A.Envelope) -> ReaderT Env IO ()
onMessage cache (m, e) = do
  case decode $ A.msgBody m :: Maybe RInput of
    Nothing -> liftIO $ putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
    Just msg -> do
      let input = toPluginInput msg
      traceUser input
      handled <- runPlugins plugins input
      unless handled $ do
        cfg <- asks config
        chan <- asks amqpChannel
        replies <- take 3 . concat <$> mapM
          (\p -> flip runReaderT cfg . runStdoutLoggingT $ p (in_from msg, in_sender msg, in_body msg))
            (newPlugins cache (in_from msg))
        sequence_ $ flip fmap replies (\b -> liftIO $ publishMessage chan (Output (in_from msg) b "privmsg"))

  liftIO $ A.ackEnv e

prPlug :: (MonadReader Config m, MonadLogger m, MonadIO m) => PluginInput -> m [String]
prPlug = prPlugin Settings
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
  } `onDomain` nixOS

defaultPlugins cache =
  [ prPlug
  , replyPlugin `onDomain` nixOS
  , commandsPlugin `onDomain` nixOS
  , nixreplPlugin `onDomain` "bottest"
  , nixpkgsPlugin cache `onDomain` "bottest"
  , tellPlugin `onDomain` nixOS
  ]

newPlugins :: (MonadLogger m, MonadReader Config m, MonadIO m) => [Text] -> String -> [ PluginInput -> m [String] ]
newPlugins cache "#nixos-unregistered" = [ unregPlugin `onDomain` nixOS ]
newPlugins cache ('#':_) = defaultPlugins cache
newPlugins cache nick = [ commandsPlugin `onDomain` ("users/" ++ nick)
                  , replyPlugin `onDomain` ("users/" ++ nick)
                  , nixreplPlugin `onDomain` ("users/" ++ nick)
                  , prPlug
                  , nixpkgsPlugin cache `onDomain` ("users/" ++ nick)
                  ]

-- Domains
nixOS = "nixOS"
testing = "Testing"
global = "Global"




examplePlugin :: Plugin
examplePlugin = Plugin
  { pluginName = "example"
  , pluginCatcher = Consumed
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
      then PassedOn else Consumed ()
  , pluginHandler = const (return ())
  }

plugins :: [Plugin]
plugins =
  [ leakedPlugin
  , karmaPlugin
  ]


