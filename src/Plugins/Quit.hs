{-# LANGUAGE OverloadedStrings #-}

module Plugins.Quit (quitPlugin) where

import           Data.Text      (Text)
import           Frontend.Types
import           Plugins

quitMessages :: [Text]
quitMessages =
  [ "exit"
  , "quit"
  , "\\quit"
  , "\\q"
  ]

quitPlugin :: Plugin
quitPlugin = Plugin
  { pluginName = "leaked"
  , pluginCatcher = \input -> case (inputMessage input `elem` quitMessages, inputSender input) of
      (True, Right (chan, _)) -> Catched False chan
      _                       -> PassedOn
  , pluginHandler = \chan -> do
      chanMsg chan "Let me do that for you"
  }
