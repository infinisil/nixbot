{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Plugins.Leaked (leakedPlugin) where

import           IRC
import           Plugins
import           Text.Regex.TDFA

regex :: Regex
regex = makeRegexOpts comp exec "nickserv identify .*" where
  comp = defaultCompOpt { caseSensitive = False }
  exec = defaultExecOpt

leakedPlugin :: Plugin
leakedPlugin = Plugin
  { pluginName = "leaked"
  , pluginCatcher = \input -> case (regex `matchTest` inputMessage input, inputChannel input) of
      (True, Just chan) -> Catched False (inputUser input, chan)
      _                 -> PassedOn
  , pluginHandler = \(user, chan) ->
      privMsg user $ "You accidentally leaked your password in #" ++ chan ++ "! "
        ++ "Make sure to change it with `/msg nickserv set password <a-new-password>` as soon as "
        ++ "possible if you don't want anybody to take over your account. `/msg nickserv help "
        ++ "set password` for more info."
  }
