{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Plugins where
import           Control.Monad.Reader
import Types
import           System.Directory
import qualified Data.Text as Text
import           System.FilePath
import qualified Data.Set as Set
import Control.Concurrent.STM
import           Config
import           IRC
import Frontend.Types
import Frontend.AMQP

class Monad m => PluginMonad m where
  getGlobalState :: m FilePath
  getChannelState :: Channel -> m FilePath
  getUserState :: User -> m FilePath
  getChannelUserState :: Channel -> User -> m FilePath
  getUser :: m User
  getChannel :: m (Maybe Channel)

data HandlerResult p = Catched Bool p
                     | PassedOn

-- TODO:
-- Initialization
data Plugin = forall p . Plugin
  { pluginName    :: String
  , pluginCatcher :: Input -> HandlerResult p
  , pluginHandler :: p -> PluginT App ()
  }

newtype PluginT m a = PluginT { unPluginT :: (FilePath, String, Input) -> m a } deriving (Functor)

instance Applicative m => Applicative (PluginT m) where
  pure a = PluginT $ \_ -> pure a
  PluginT a <*> PluginT b = PluginT $ \args -> let af = a args; bf = b args in af <*> bf

instance Monad m => Monad (PluginT m) where
  PluginT a >>= f = PluginT $ \args -> let
    af = a args
    in af >>= (\y -> unPluginT (f y) args)

instance MonadIO m => MonadIO (PluginT m) where
  liftIO action = PluginT $ \_ -> liftIO action

instance MonadTrans PluginT where
  lift a = PluginT $ const a

instance MonadIO m => PluginMonad (PluginT m) where
  getGlobalState = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "global" </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getChannelState channel = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "channel" </> Text.unpack channel </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getUserState user = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "user" </> Text.unpack user </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getChannelUserState channel user = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "channel-user" </> Text.unpack channel </> Text.unpack user </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getUser = PluginT $ \(_, _, Input { inputSender }) -> case inputSender of
    Left user -> return user
    Right (_, user) -> return user
  getChannel = PluginT $ \(_, _, Input { inputSender }) -> case inputSender of
    Left _ -> return Nothing
    Right (chan, _) -> return $ Just chan

instance PluginMonad m => PluginMonad (ReaderT r m) where
  getGlobalState = lift getGlobalState
  getChannelState channel = lift $ getChannelState channel
  getUserState user = lift $ getUserState user
  getChannelUserState channel user = lift $ getChannelUserState channel user
  getUser = lift getUser
  getChannel = lift getChannel

runPlugins :: [Plugin] -> Input -> App Bool
runPlugins [] _ = return False
runPlugins (Plugin { pluginName, pluginCatcher, pluginHandler }:ps) input = case pluginCatcher input of
  PassedOn -> runPlugins ps input
  Catched absorbed p -> do
    cfg <- asks config
    unPluginT (pluginHandler p) (configStateDir cfg </> "new", pluginName, input)
    if absorbed then return True else runPlugins ps input

privMsg :: User -> Message -> PluginT App ()
privMsg user msg = lift $ sendFrontend Output
  { outputReceiver = Left user
  , outputMessage = msg
  }

chanMsg :: Channel -> Message -> PluginT App ()
chanMsg chan msg = lift $ sendFrontend Output
  { outputReceiver = Right chan
  , outputMessage = msg
  }


isKnown :: User -> App Bool
isKnown user = do
  stateVar <- asks sharedState
  state <- lift $ readTVarIO stateVar
  return $ Set.member user (knownUsers state)

reply :: Message -> PluginT App ()
reply msg = do
  chan <- getChannel
  case chan of
    Nothing -> do
      user <- getUser
      privMsg user msg
    Just ch -> chanMsg ch msg
