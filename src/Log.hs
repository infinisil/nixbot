{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Log
  ( withLogging
  , logMsg
  , logMsgEnv
  , show'
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Monad.Reader
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.IO                   as TIO
import           System.IO
import           Types

logger :: App (Async ())
logger = do
  lift $ hSetBuffering stdout LineBuffering
  queue <- asks logQueue
  lift $ async $ readLog queue
  where
    readLog :: TMQueue Text -> IO ()
    readLog queue = do
      mmsg <- atomically $ readTMQueue queue
      case mmsg of
        Nothing -> do
          TIO.putStrLn "Finished logging, logger queue empty and closed"
          return ()
        Just msg -> do
          TIO.putStrLn msg
          readLog queue

logMsg :: Text -> App ()
logMsg msg = do
  queue <- asks logQueue
  lift $ atomically $ writeTMQueue queue msg

logMsgEnv :: Env -> Text -> IO ()
logMsgEnv Env { logQueue } msg = atomically $ writeTMQueue logQueue msg

show' :: Show a => a -> Text
show' = Text.pack . show

withLogging :: App a -> App a
withLogging app = do
  thread <- logger

  result <- app

  queue <- asks logQueue
  lift $ atomically $ closeTMQueue queue
  lift $ wait thread
  return result
