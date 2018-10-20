{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.State    (MonadState, evalStateT)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (pack)
import           Nix.Session
import           Nix.Session.Config
import           System.Directory
import           System.FilePath

import           Control.Monad.IO.Class (MonadIO, liftIO)

main :: IO ()
main = do
  nixInstantiate <- fromMaybe (error "Couldn't find nix-instantiate executable") <$> findExecutable "nix-instantiate"
  config <- getConfig nixInstantiate
  let env = Env { _nixInstantiate = nixInstantiate
                , _globalConfig = config
                }
  runReaderT (evalStateT thething startState) env


thething :: (MonadIO m, MonadState SessionState m, MonadReader Env m) => m ()
thething = do
  line <- pack <$> liftIO getLine
  result <- processText line
  liftIO $ putStrLn result
  thething

getConfig :: FilePath -> IO GlobalConfig
getConfig nixInstantiate = do
  home <- getHomeDirectory
  let configPath = home </> ".config/nix-session/config.nix"
  exists <- doesFileExist configPath
  evalConfig nixInstantiate $ if exists then configPath else "{}"
