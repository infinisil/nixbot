{-# LANGUAGE FlexibleContexts      #-}
module Plugins.Hello (helloPlugin) where

import Plugins

helloPlugin :: Monad m => MyPlugin () m
helloPlugin = MyPlugin () trans "hello"
  where
    trans (nick, "hello!") = return [ "Hello, " ++ nick ++ "!" ]
    trans _                = return []
