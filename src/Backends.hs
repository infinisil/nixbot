{-# LANGUAGE OverloadedStrings #-}

module Backends where

import           Data.Conduit
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Text                (Text)

import           Control.Concurrent       (threadDelay)
import           Control.Monad.State
import           Control.Monad.Writer

import           System.IO

import qualified Data.Conduit.Combinators as C

data FrontendInput = FrontendInput
  { inputChannel :: Text
  , inputMessage :: Text
  , inputNick    :: Text
  } deriving Show

data FrontendOutput = FrontendOutput
  { outputChannel :: Text
  , outputMessage :: Text
  } deriving Show


type TheBot = ConduitT FrontendInput FrontendOutput

linedStdin :: MonadIO m => ConduitT i String m ()
linedStdin = do
  eof <- liftIO isEOF
  if eof then return ()
  else do
    liftIO getLine >>= yield
    linedStdin


wizard :: MonadIO m => ConduitT String o m ()
wizard = do
  liftIO $ putStrLn "Try patching the source (Y/N)"
  tof <- await
  case tof of
    Nothing  -> return ()
    Just "Y" -> return ()
    Just "N" -> do
      liftIO $ putStrLn "Does it use dlopen? (Y/N)"
      dlopen <- await
      return ()
    Just _ -> do
      liftIO $ putStr "That's not a valid answer.. "
      wizard



myBot :: TheBot IO ()
myBot = do
  message <- await
  case message of
    Nothing -> return ()
    Just msg -> do
      liftIO $ print msg
      myBot
