{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.State      (StateT (..), evalStateT, runStateT)
import           Control.Monad.Trans      (lift)
import           Data.Maybe               (fromMaybe)
import           Data.Text                (pack)
import           Nix.Session
import           Nix.Session.Config
import           System.Console.Haskeline
import           System.Directory
import           System.FilePath

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'
main :: IO ()
main = do
  nixInstantiate <- fromMaybe (error "Couldn't find nix-instantiate executable") <$> findExecutable "nix-instantiate"
  config <- getConfig nixInstantiate
  menv <- initEnv config
  case menv of
    Left err  -> fail err
    Right env -> evalStateT (runInputT defaultSettings (withInterrupt thething)) env

thething :: InputT (StateT Env IO) ()
thething = handleInterrupt thething $ do
  minput <- getInputLine "> "
  case minput of
    Nothing -> lift saveEnv
    Just input -> do
      result <- lift $ processText (pack input)
      outputStr result
      thething

getConfig :: FilePath -> IO GlobalConfig
getConfig nixInstantiate = do
  home <- getHomeDirectory
  let configPath = home </> ".config/nix-session/config.nix"
  exists <- doesFileExist configPath
  evalConfig nixInstantiate $ if exists then configPath else "{}"
