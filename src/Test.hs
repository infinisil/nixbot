{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plugins where

import Control.Monad.IO.Class
import Control.Monad.State 
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Logger
import System.IO

type Input = String
type Output = String

data Plugin s m = Plugin
  { initial :: s
  , handler :: Input -> StateT s m Output
  }
  
incrementPlugin :: (MonadLogger m, Monad m) => Plugin Int m
incrementPlugin = Plugin 0 trans
  where
    trans input = do
      modify (+1)
      return "Increased value"
      
togglePlugin :: Monad m => Plugin Bool m
togglePlugin = Plugin False trans
  where
    trans input = do
      modify not
      return "Flipped value"

{-

My goal would be to specify plugins like

  trans :: (MonadIO m, MonadLogger m) => Input -> m Output

for a plugin transformation that can use IO, logging and has a String state,
but I haven't found a way for this to work. The forall makes it possible to
not have Plugin not depend on m, which is needed if multiple plugins with
different state's are to be declared in a list (see below), but then I can't
add additional typeclasses to trans

-}

data Backend s m = Backend
  { load :: m s
  , store :: s -> m ()
  }

fileBackend :: (Read s, Show s, MonadIO m) => FilePath -> Backend s m
fileBackend path = Backend
  { load = liftIO $ readFile path >>= readIO
  , store = liftIO . writeFile path . show
  }

memoryBackend :: MonadState s m => Backend s m
memoryBackend = Backend
  { load = get
  , store = put
  }

runPluginWithBackend :: (Monad m) => Plugin s m -> Backend s m -> Input -> m Output
runPluginWithBackend (Plugin init trans) (Backend load store) input = do
  state <- load -- The initial state would be used here if the load doesn't succeed
  (result, newState) <- runStateT (trans input) state
  store newState
  return result

plugins :: (Monad m, MonadIO m, MonadLogger m, MonadState Bool m) => [ Input -> m Output ]
plugins =
  [ runPluginWithBackend incrementPlugin (fileBackend "count")
  , runPluginWithBackend togglePlugin memoryBackend
  ]
