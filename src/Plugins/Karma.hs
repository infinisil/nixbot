{-# LANGUAGE FlexibleContexts #-}
module Plugins.Karma (karmaPlugin) where

import           Plugins

import           Control.Monad.State
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes, listToMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Text.Regex.TDFA     ((=~))

karmaRegex :: String
karmaRegex = "([^[:space:]]+)\\+\\+"

mods :: String -> [String] -> State (M.Map String Int) [(String, Bool)]
mods = undefined

blacklistedUsers = Set.fromList [ "c", "C", "g", "avr-g" ]

karmaPlugin :: Monad m => MyPlugin (M.Map String Int) m
karmaPlugin = MyPlugin M.empty trans "karma"
  where
    trans (chan, nick, msg) = do
      let matches = fmap (!!1) (msg =~ karmaRegex :: [[String]])
      mmessages <- forM matches $ \user -> if user `Set.member` blacklistedUsers then return Nothing else do
        let decrease = user == nick
        let mod = if decrease then (\x -> x - 1) else (+1)
        oldMap <- get
        let newKarma = mod . M.findWithDefault 0 user $ oldMap
        modify $ M.insert user newKarma
        return $ Just $ user ++ "'s karma got " ++
          (if decrease then "decreased" else "increased")
          ++ " to " ++ show newKarma
      let messages = catMaybes mmessages
      return [ intercalate ", " messages | not $ null messages ]
