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
import           System.IO

import           Control.Monad.IO.Class (MonadIO, liftIO)

main :: IO ()
main = do
  nixInstantiate <- fromMaybe (error "Couldn't find nix-instantiate executable") <$> findExecutable "nix-instantiate"
  config <- getConfig nixInstantiate
  menv <- initEnv config
  putStr "> "
  case menv of
    Left err  -> fail err
    Right env -> evalStateT thething env


thething :: (MonadIO m, MonadState Env m) => m ()
thething = do
  eof <- liftIO isEOF
  if eof then saveEnv
  else do
    line <- pack <$> liftIO getLine
    result <- processText line
    liftIO $ putStr result
    liftIO $ putStr "> "
    thething

getConfig :: FilePath -> IO GlobalConfig
getConfig nixInstantiate = do
  home <- getHomeDirectory
  let configPath = home </> ".config/nix-session/config.nix"
  exists <- doesFileExist configPath
  evalConfig nixInstantiate $ if exists then configPath else "{}"
