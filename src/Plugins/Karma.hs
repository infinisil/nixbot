{-# LANGUAGE FlexibleContexts #-}
module Plugins.Karma (karmaPlugin) where

import           Plugins

import           Control.Monad.State
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (listToMaybe)
import           Text.Regex.TDFA     ((=~))

karmaRegex :: String
karmaRegex = "([^[:space:]]+)\\+\\+"

mods :: String -> [String] -> State (M.Map String Int) [(String, Bool)]
mods = undefined

karmaPlugin :: Monad m => MyPlugin (M.Map String Int) m
karmaPlugin = MyPlugin M.empty trans "karma"
  where
    trans (chan, nick, msg) = do
      let matches = fmap (!!1) (msg =~ karmaRegex :: [[String]])
      messages <- forM matches $ \user -> do
        let decrease = user == nick
        let mod = if decrease then (\x -> x - 1) else (+1)
        oldMap <- get
        let newKarma = mod . M.findWithDefault 0 user $ oldMap
        modify $ M.insert user newKarma
        return $ user ++ "'s karma got " ++
          (if decrease then "decreased" else "increased")
          ++ " to " ++ show newKarma
      return [ intercalate ", " messages | not $ null messages ]
