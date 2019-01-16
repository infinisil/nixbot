{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Plugins.Unreg (unregPlugin) where

import           Plugins

message :: String
message = "Hello! You've been sent to #nixos-unregistered due to the recent spam attacks on Freenode. Please register and identify with NickServ to join #nixos, see https://freenode.net/kb/answer/registration. Sorry for the inconvenience!"

unregPlugin :: Plugin
unregPlugin = Plugin
  { pluginName = "unreg"
  , pluginCatcher = \Input { inputChannel, inputUser } -> case inputChannel of
      Just "nixos-unregistered" -> Catched True inputUser
      _                         -> PassedOn
  , pluginHandler = \user ->
      reply $ user ++ ": " ++ message
  }
