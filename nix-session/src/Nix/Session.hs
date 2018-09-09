{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Nix.Session where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)


data Assignment = Assignment
  { ident        :: Text
  , expression   :: Text
  , dependencies :: [Assignment]
  }

start :: Map Text Assignment
start = Map.empty

foodefined = Map.singleton "foo" (Assignment "foo" "1" [])

data ReadOnly
data ReadWrite

data Request = Ping
             deriving Show
data Reply = Pong
           deriving Show

data SessionState = SessionState

runCommand :: Request -> State SessionState Reply
runCommand Ping = return Pong



