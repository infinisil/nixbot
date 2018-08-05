{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Plugins.Tell (tellPlugin) where

import           Plugins

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.List
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes)
import           Data.Time

import           Utils

type Chan = String
type Nick = String

data Entry = Entry
  { from :: Nick
  , chan :: String
  , msg  :: String
  , time :: UTCTime
  } deriving (Show, Read)

formatEntry :: UTCTime -> Entry -> String
formatEntry now Entry { from, msg, time } = ago ++ " ago <" ++ from ++ "> " ++ msg
  where ago = prettySeconds 2 . round . diffUTCTime now $ time


tellPlugin :: MonadIO m => MyPlugin (Map (Chan, Nick) [Entry]) m
tellPlugin = MyPlugin M.empty trans "tell"
  where
    trans (chan, nick, input) = do
      inbox <- gets $ M.findWithDefault [] (chan, nick)
      modify $ M.delete (chan, nick)
      time <- liftIO getCurrentTime
      let messages = map (formatEntry time) inbox
      tell <- case words input of
        [",tell"] -> return [ "Use `,tell john Remember to do the laundry` to send this to john next time he's talking in this channel" ]
        ",tell":target:rest -> do
          let entry = Entry {
                  from = nick
                , chan = chan
                , msg = unwords rest
                , time = time
                }
          modify $ M.insertWith (flip (++)) (chan, target) [entry]
          return ["I'll pass that on to " ++ target]
        _ -> return []
      return $ map (\m -> nick ++ ": " ++ m) $ messages ++ tell
