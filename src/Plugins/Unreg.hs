{-# LANGUAGE FlexibleContexts #-}
module Plugins.Unreg (unregPlugin) where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Time
import           Plugins

message = "Hello! You've been sent to #nixos-unregistered due to the recent spam attacks on Freenode. Please register and identify with NickServ to join #nixos, see https://freenode.net/kb/answer/registration. Sorry for the inconvenience!"

unregPlugin :: MonadIO m => MyPlugin (Map String UTCTime) m
unregPlugin = MyPlugin Map.empty trans "unreg"
  where
    trans (chan, nick, msg) = do
      now <- liftIO getCurrentTime
      mtime <- gets $ Map.lookup nick
      let sayit = maybe True (\time -> now `diffUTCTime` time > 60) mtime
      if sayit then do
        modify $ Map.insert nick now
        return [ nick ++ ": " ++ message ]
      else return []
