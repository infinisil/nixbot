{-# LANGUAGE FlexibleContexts #-}

module Plugins where
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.State.Class

import           Data.Maybe                (fromMaybe)
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict          as S
import           Text.Read                 (readMaybe)

import           Config


type PluginInput = (String, String)
data MyPlugin s m = MyPlugin { initState :: s
                             , transf    :: PluginInput -> StateT s m [String]
                             , name      :: String
                             }

data Backend s m = Backend { load :: m (Maybe s), store :: s -> m () }

fileBackend :: (Read s, MonadLogger m, Show s, MonadIO m, MonadReader Config m) => FilePath -> Backend s m
fileBackend path = Backend
  { load = do
      statePath <- reader stateDir'
      let fullPath = statePath </> path

      liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)
      fileExists <- liftIO $ doesFileExist fullPath
      if not fileExists
        then return Nothing
        else do
          contents <- liftIO $ S.readFile fullPath
          return $ readMaybe contents
  , store = \s -> do
      statePath <- reader stateDir'
      let fullPath = statePath </> path
      liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)
      liftIO . writeFile fullPath $ show s
  }

examplePlugin :: Monad m => MyPlugin Int m
examplePlugin = MyPlugin 0 trans "example"
  where
    trans input = do
      value <- get
      put $ value + 1
      return [ "increased value by 1" ]

runPlugin :: MonadLogger m => MyPlugin s m -> Backend s m -> PluginInput -> m [String]
runPlugin (MyPlugin init trans name) (Backend load store) input = do
  state <- fromMaybe init <$> load
  (results, newState) <- runStateT (trans input) state
  store newState
  return results

type RunnablePlugin m = PluginInput -> m [String]

onDomain :: (MonadLogger m, MonadIO m, MonadReader Config m, Read s, Show s) => MyPlugin s m -> String -> RunnablePlugin m
onDomain plugin domain = runPlugin plugin (fileBackend (domain ++ "/" ++ name plugin))
