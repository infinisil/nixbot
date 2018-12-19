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
import           Plugins.Nixpkgs
import           Plugins.NixRepl
import           Plugins.Pr
import           Plugins.Reply
import           Plugins.Tell
import           Plugins.Unreg
import           Types

import           Control.Concurrent              (forkIO)
import           Control.Concurrent.STM          (TMVar, atomically,
                                                  newEmptyTMVarIO, newTMVarIO,
                                                  putTMVar, takeTMVar)
import           Control.Exception               (SomeException, handle)
import           Control.Exception.Base          (IOException)
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
import           Data.Monoid                     ((<>))
import           Data.Ord                        (comparing)
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

main = do
  hSetBuffering stdout LineBuffering

  config <- getConfig
  conn <- A.openConnection'' (amqpOptions config)
  chan <- A.openChannel conn

  runReaderT (runStderrLoggingT start) $ Env config chan

  var <- liftIO newEmptyTMVarIO
  A.addConnectionClosedHandler conn True $ atomically $ putTMVar var ()
  atomically $ takeTMVar var

start :: (MonadReader Env m, MonadLogger m, MonadIO m) => m ()
start = do
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
  tag <- liftIO $ A.consumeMsgs chan myQueue A.Ack (\x -> runReaderT (onMessage cache x) r)
  logDebugN $ "Started consumer with tag " <> pack (show tag)

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
  privMsg user message = ReaderT $ \(Env _ chan) -> do
    let output = encode $ Output user message "privmsg"
    liftIO $ A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = output
      , A.msgDeliveryMode = Just A.Persistent
      }
    return ()
  chanMsg channel message = ReaderT $ \(Env _ chan) -> do
    let output = encode $ Output ('#':channel) message "privmsg"
    liftIO $ A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = output
      , A.msgDeliveryMode = Just A.Persistent
      }
    return ()

onMessage :: [Text] -> (A.Message, A.Envelope) -> ReaderT Env IO ()
onMessage cache (m, e) = do
  case decode $ A.msgBody m :: Maybe RInput of
    Nothing -> liftIO $ putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
    Just msg -> runPlugins plugins (toPluginInput msg)
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
  [ karmaPlugin `onDomain` nixOS
  , prPlug
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
                  , karmaPlugin `onDomain` ("users/" ++ nick)
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

plugins :: [Plugin]
plugins =
  [ examplePlugin
  ]


