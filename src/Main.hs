{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}


module Main where

import           Config
import           Plugins
import           Plugins.Commands
import           Plugins.Hello
import           Plugins.Karma
import           Plugins.Nixpkgs
import           Plugins.NixRepl
import           Plugins.Pr

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


data Input = Input
  { in_from   :: String
  , in_body   :: String
  , in_sender :: String
  } deriving (Show, Generic)

data Output = Output
  { out_target       :: String
  , out_body         :: String
  , out_message_type :: String
  } deriving (Show, Generic)

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

data Env = Env
  { config     :: Config
  , connection :: A.Connection
  }

main = do
  hSetBuffering stdout LineBuffering
  config <- getConfig
  runStdoutLoggingT $ runReaderT server config

writeDone :: TMVar () -> IO ()
writeDone var = atomically $ putTMVar var ()

server :: (MonadError IOException m, MonadLogger m, MonadReader Config m, MonadIO m) => m ()
server = do
  opts <- reader amqpOptions
  conn <- liftIO $ A.openConnection'' opts
  config <- ask
  runReaderT (catchError start onInterrupt) $ Env config conn

start :: (MonadReader Env m, MonadLogger m, MonadIO m) => m ()
start = do
  $(logDebug) "Got connection"
  conn <- reader connection
  chan <- liftIO $ A.openChannel conn
  $(logDebug) "Got channel"

  (publishQueueName, _, _) <- liftIO $ A.declareQueue chan $ A.newQueue
    { A.queueName = "queue-publish"
    , A.queuePassive = True
    }
  $(logDebug) $ "Declared queue with name " <> pack (show publishQueueName)

  (myQueue, _, _) <- liftIO $ A.declareQueue chan $ A.newQueue
    { A.queueName = ""
    , A.queueAutoDelete = True
    , A.queueExclusive = True
    }
  $(logDebug) $ "Declared my queue with name " <> pack (show myQueue)

  liftIO $ A.declareExchange chan exchange
  $(logDebug) "Declared exchange"

  liftIO $ A.bindQueue chan myQueue "exchange-messages" "queue-publish"
  $(logDebug) "Bound queue"

  liftIO $ A.confirmSelect chan False
  $(logDebug) "disabled nowait"

  cfg <- reader config
  tag <- liftIO $ A.consumeMsgs chan myQueue A.Ack (onMessage cfg chan)
  $(logDebug) $ "Started consumer with tag " <> pack (show tag)

  var <- liftIO newEmptyTMVarIO
  liftIO $ A.addConnectionClosedHandler conn True $ writeDone var
  $(logDebug) "terminating if enter is pressed..."
  liftIO . atomically $ takeTMVar var

onInterrupt :: (MonadReader Env m, MonadLogger m, MonadIO m) => IOException -> m ()
onInterrupt e = do
  $(logInfo) "Interrupted, closing connection"
  conn <- reader connection
  liftIO $ A.closeConnection conn

publishMessage :: A.Channel -> Output -> IO (Maybe Int)
publishMessage chan msg = do
  putStrLn $ "Sending the message " <> show msg
  -- publish a message to our new exchange
  intMb <- A.publishMsg chan "" "queue-publish" A.newMsg
    { A.msgBody = encode msg
    , A.msgDeliveryMode = Just A.Persistent}

  putStrLn $ "Published Message" <> maybe "" (\s -> ", got sequence number " ++ show s) intMb
  return intMb

onMessage :: Config -> A.Channel -> (A.Message, A.Envelope) -> IO ()
onMessage cfg chan (m, e) =
  case decode $ A.msgBody m :: Maybe Input of
    Nothing -> do
      putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
      A.ackEnv e
    Just msg -> do
      putStrLn $ "Valid message " ++ show msg
      forkIO $ do
        replyBodies <- reply cfg msg
        putStrLn $ "got replies: " ++ concatMap show replyBodies
        sequence_ $ flip fmap replyBodies (\b -> publishMessage chan (Output (in_from msg) b "privmsg"))
      A.ackEnv e

reply :: Config -> Input -> IO [String]
reply cfg Input { in_from = channel, in_sender = nick, in_body = msg } = do
  let chanPlugs = newPlugins channel
  replies <- mapM (\p -> flip runReaderT cfg . runStdoutLoggingT $ p (nick, msg)) chanPlugs
  return $ take 3 $ concat replies

newPlugins :: (MonadLogger m, MonadReader Config m, MonadIO m) => String -> [ PluginInput -> m [String] ]
newPlugins "#nixos" = [ karmaPlugin `onDomain` nixOS
                      , prPlugin `onDomain` nixOS
                      , commandsPlugin `onDomain` nixOS
                      , nixreplPlugin `onDomain` "bottest"
                      , nixpkgsPlugin `onDomain` "bottest"
                      ]
newPlugins "#nixos-chat" = [ karmaPlugin `onDomain` nixOS
                      , prPlugin `onDomain` nixOS
                      , commandsPlugin `onDomain` nixOS
                      , nixreplPlugin `onDomain` "bottest"
                      , nixpkgsPlugin `onDomain` "bottest"
                      ]
newPlugins "#bottest" = [ karmaPlugin `onDomain` nixOS
                        , prPlugin `onDomain` nixOS
                        , helloPlugin `onDomain` nixOS
                        , commandsPlugin `onDomain` nixOS
                        , nixpkgsPlugin `onDomain` nixOS
                        , nixreplPlugin `onDomain` "bottest"
                        ]
newPlugins "#nixos-borg" = [ karmaPlugin `onDomain` nixOS
                           , prPlugin `onDomain` nixOS
                           , helloPlugin `onDomain` nixOS
                           , commandsPlugin `onDomain` nixOS
                           , nixreplPlugin `onDomain` "bottest"
                           ]

newPlugins "#nixos-dev" = [ karmaPlugin `onDomain` nixOS
                           , prPlugin `onDomain` nixOS
                           , commandsPlugin `onDomain` nixOS
                           , nixreplPlugin `onDomain` "bottest"
                           ]
newPlugins ('#':_) = []
newPlugins nick = [ commandsPlugin `onDomain` ("users/" ++ nick)
                  , helloPlugin `onDomain` ("users/" ++ nick)
                  , karmaPlugin `onDomain` ("users/" ++ nick)
                  ]

-- Domains
nixOS = "nixOS"
testing = "Testing"
global = "Global"


