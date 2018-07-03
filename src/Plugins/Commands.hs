{-# LANGUAGE FlexibleContexts #-}
module Plugins.Commands (commandsPlugin) where

import           Control.Monad.IO.Class
import           Data.List              (intercalate, sortBy)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)
import           Plugins
import           System.Exit
import           System.Process
import           Text.EditDistance      (defaultEditCosts, levenshteinDistance)


import           Control.Monad.State

data LookupResult = Empty | Exact String | Guess String String

replyLookup :: String -> Maybe String -> LookupResult -> [String]
replyLookup _ _ Empty = []
replyLookup _ Nothing (Exact str) = [str]
replyLookup _ (Just arg) (Exact str) = [arg ++ ": " ++ str]
replyLookup nick Nothing (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", str]
replyLookup nick (Just arg) (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", arg ++ ": " ++ str]

lookupCommand :: String -> M.Map String String -> LookupResult
lookupCommand str map = result
  where
    filtered =
      filter ((3 >=) . snd)
      . sortBy (comparing snd)
      . fmap (\s -> (s,
                     levenshteinDistance defaultEditCosts str s)
             )
      . filter (\s -> (s == str) || ((>=3) . length $ s))
      $ if length str <= 3 then (if M.member str map then [str] else []) else M.keys map
    result = case filtered of
               []         -> Empty
               (str, 0):_ -> Exact . fromJust $ M.lookup str map
               (str, _):_ -> Guess str . fromJust $ M.lookup str map

nixLocate :: MonadIO m => String -> m (Either String [String])
nixLocate file = do
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "/run/current-system/sw/bin/nix-locate"
    [ "--minimal"
    , "--top-level"
    , "--whole-name"
    , file
    ] ""
  case exitCode of
    ExitFailure code -> return $ Left $ "nix-locate: Error(" ++ show code ++ "): " ++ show stderr ++ show stdout
    ExitSuccess -> return $ Right $ lines stdout

commandsPlugin :: MonadIO m => MyPlugin (Map String String) m
commandsPlugin = MyPlugin M.empty trans "commands"
  where
    trans (nick, ',':command) = case words command of
      [] -> do
        keys <- gets M.keys
        return ["All commands: " ++ unwords keys]
      "locate":args -> case args of
        [] -> return ["Use ,locate <filename> to find packages containing such a file. Powered by nix-index (local installation recommended)."]
        [arg] -> do
          packages <- nixLocate arg
          return $ case packages of
            Left error -> [error]
            Right [] -> ["Couldn't find any packages"]
            Right packages -> ["Found packages: " ++ intercalate ", " packages]
        _ -> return [",locate only takes 1 argument"]
      [ cmd ] -> do
        result <- gets (lookupCommand cmd)
        return $ replyLookup nick Nothing result
      cmd:"=":rest -> case length rest of
          0 -> do
            modify (M.delete cmd)
            return [ cmd ++ " undefined" ]
          _ -> do
            modify (M.insert cmd (unwords rest))
            return [ cmd ++ " defined" ]
      cmd:args -> do
        result <- gets (lookupCommand cmd)
        return $ replyLookup nick (Just (unwords args)) result
    trans (_, _) = return []
