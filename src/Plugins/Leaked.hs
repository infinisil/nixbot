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
  , pluginCatcher = \input -> if regex `matchTest` inputMessage input then Consumed input else PassedOn
  , pluginHandler = \input@Input { inputUser, inputChannel = Just channel } ->
      privMsg inputUser $ "You accidentally leaked your password in #" ++ channel ++ "! "
        ++ "Make sure to change it with `/msg nickserv set password <a-new-password>` as soon as "
        ++ "possible if you don't want anybody to take over your account. `/msg nickserv help "
        ++ "set password` for more info."

  }
  where
