{-# LANGUAGE FlexibleContexts      #-}
module Plugins.Karma (karmaPlugin) where

import Plugins

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (listToMaybe)
import           Text.Regex.TDFA                 ((=~))

karmaRegex :: String
karmaRegex = "\\`[[:space:]]*([^[:space:]]+)[[:space:]]*\\+\\+\\'"

karmaPlugin :: Monad m => MyPlugin (M.Map String Int) m
karmaPlugin = MyPlugin M.empty trans "karma"
  where
    trans (nick, msg) =
      case matches of
        Nothing   -> return []
        Just user -> do
          let decrease = user == nick
          let mod = if decrease then (\x -> x - 1) else (\x -> x + 1)
          oldMap <- get
          let newKarma = mod . M.findWithDefault 0 user $ oldMap
          modify (M.insert user newKarma)
          return [ user ++ "'s karma got " ++
            (if decrease then "decreased" else "increased")
            ++ " to " ++ show newKarma ]
      where
        matches = listToMaybe $ fmap (!!1) (msg =~ karmaRegex :: [[String]])
