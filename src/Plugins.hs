{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Plugins where
import           Control.Monad.Logger
import           Control.Monad.Reader
import Types
import           System.Directory
import           System.FilePath
import           Config
import           IRC

data Input = Input
  { inputUser    :: User
  , inputChannel :: Maybe Channel
  , inputMessage :: Message
  } deriving (Show)

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
  , pluginHandler :: forall m . (MonadReader Config m, MonadLogger m, IRCMonad m, MonadIO m, PluginMonad m) => p -> m ()
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

instance MonadLogger m => MonadLogger (PluginT m) where

instance IRCMonad m => IRCMonad (PluginT m) where
  privMsg user msg = lift $ privMsg user msg
  chanMsg channel msg = lift $ chanMsg channel msg
  isKnown user = lift $ isKnown user

instance MonadIO m => PluginMonad (PluginT m) where
  getGlobalState = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "global" </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getChannelState channel = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "channel" </> channel </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getUserState user = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "user" </> user </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getChannelUserState channel user = PluginT $ \(base, pluginName, _) -> do
    let dir = base </> "channel-user" </> channel </> user </> pluginName
    liftIO $ createDirectoryIfMissing True dir
    return dir
  getUser = PluginT $ \(_, _, Input { inputUser }) -> return inputUser
  getChannel = PluginT $ \(_, _, Input { inputChannel }) -> return inputChannel

instance PluginMonad m => PluginMonad (ReaderT r m) where
  getGlobalState = lift getGlobalState
  getChannelState channel = lift $ getChannelState channel
  getUserState user = lift $ getUserState user
  getChannelUserState channel user = lift $ getChannelUserState channel user
  getUser = lift getUser
  getChannel = lift getChannel

runPlugins :: (MonadLogger m, MonadReader Env m, MonadIO m, IRCMonad m) => [Plugin] -> Input -> m Bool
runPlugins [] _ = return False
runPlugins (Plugin { pluginName, pluginCatcher, pluginHandler }:ps) input = case pluginCatcher input of
  PassedOn -> runPlugins ps input
  Catched absorbed p -> do
    cfg <- asks config
    unPluginT (runReaderT (pluginHandler p) cfg) (stateDir cfg </> "new", pluginName, input)
    if absorbed then return True else runPlugins ps input

reply :: (IRCMonad m, PluginMonad m) => Message -> m ()
reply msg = getChannel >>= \case
  Nothing -> do
    user <- getUser
    privMsg user msg
  Just chan -> chanMsg chan msg
