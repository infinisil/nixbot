{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Unreg (unregPlugin) where

import           Data.Text      (Text)
import           Frontend.Types
import           Plugins

message :: Text
message = "Hello! You've been sent to #nixos-unregistered due to the recent spam attacks on Freenode. Please register and identify with NickServ to join #nixos, see https://freenode.net/kb/answer/registration. Sorry for the inconvenience!"

unregPlugin :: Plugin
unregPlugin = Plugin
  { pluginName = "unreg"
  , pluginCatcher = \Input { inputSender } -> case inputSender of
      Right ("nixos-unregistered", user) -> Catched True user
      _                                  -> PassedOn
  , pluginHandler = \user ->
      reply $ user <> ": " <> message
  }
