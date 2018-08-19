{-# LANGUAGE FlexibleContexts #-}
module Plugins.Reply (replyPlugin) where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Time
import           Plugins

transform = map toLower . filter isAlpha

replies :: [(String, [String])]
replies = map (fmap (map transform))
  [ ( "Night!",
    [ "night"
    , "good night"
    , "gnight"
    ])
  , ( "Good luck!",
    [ "wish me luck"
    ])
  ]

getReply :: String -> Maybe String
getReply msg = fst <$> find matches replies
  where
    transformed = transform msg
    matches (_, list) = transformed `elem` list

replyPlugin :: MonadIO m => MyPlugin (Map String UTCTime) m
replyPlugin = MyPlugin Map.empty trans "reply"
  where
    trans (chan, nick, msg) = case getReply msg of
      Nothing -> return []
      Just reply -> do
        now <- liftIO $ getCurrentTime
        mtime <- gets $ Map.lookup reply
        let sayit = maybe True (\time -> now `diffUTCTime` time > 60 * 5) mtime
        if sayit then do
          modify $ Map.insert reply now
          return [ reply ]
        else return []

    trans _                      = return []
