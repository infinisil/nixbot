{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.State    (MonadState, evalStateT)
import           Data.Text              (pack)
import           Nix.Session

import           Control.Monad.IO.Class (MonadIO, liftIO)

main :: IO ()
main = evalStateT thething startState


thething :: (MonadIO m, MonadState NixEnv m) => m ()
thething = do
  line <- pack <$> liftIO getLine
  result <- processText line
  liftIO $ putStrLn result
  thething
