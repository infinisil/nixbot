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

type Chan = String
type Nick = String

data Entry = Entry
  { from :: Nick
  , chan :: String
  , msg  :: String
  , time :: UTCTime
  } deriving (Show, Read)

prettySeconds :: Int -> Integer -> String
prettySeconds relevant seconds = intercalate ", " relevantStrings
  where
    relevantStrings = take relevant . catMaybes $ format "year" rest : strings

    (rest, strings) = mapAccumR next seconds
      [ (52, "week")
      , (7, "day")
      , (24, "hour")
      , (60, "minute")
      , (60, "second")
      ]

    next val (unit, str) = format str <$> divMod val unit

    format _ 0   = Nothing
    format str 1 = Just $ "1 " ++ str
    format str n = Just $ show n ++ " " ++ str ++ "s"

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
          modify $ M.insertWith (++) (chan, target) [entry]
          return ["I'll pass that on to " ++ target]
        _ -> return []
      return $ map (\m -> nick ++ ": " ++ m) $ messages ++ tell
