{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Plugins.Commands.Tell (Tell, tellParser, tellHandle, tellSnooper) where

import           Plugins

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Aeson
import           Data.Char              (isSpace)
import           Data.Functor           (($>))
import           Data.Time
import           Data.Void
import           GHC.Generics           (Generic)
import           IRC
import           System.Directory
import           System.FilePath
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Utils

type Parser = Parsec Void String

parseWord :: Parser String
parseWord = some (satisfy (not . isSpace)) <* (void space1 <|> eof)

data Entry = Entry
  { from :: User
  , chan :: String
  , msg  :: String
  , time :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Entry
instance ToJSON Entry

formatEntry :: UTCTime -> Entry -> String
formatEntry now Entry { from, msg, time } = ago ++ " ago <" ++ from ++ "> " ++ msg
  where ago = prettySeconds 2 . round . diffUTCTime now $ time


data Tell = TellHelp
          | TellCommand User Message
          deriving (Show)

tellParser :: Parser Tell
tellParser = eof $> TellHelp
  <|> TellCommand <$> parseWord <*> getInput

tellHandle :: (MonadIO m, PluginMonad m, IRCMonad m) => Tell -> m ()
tellHandle TellHelp = reply "Use `,tell john Remember to do the laundry` to send this to john next time he's talking in this channel"
tellHandle (TellCommand target message) = getChannel >>= \case
  Nothing -> reply ",tell doesn't work in private messages"
  Just chan -> do
    time <- liftIO getCurrentTime
    nick <- getUser
    let entry = Entry
          { from = nick
          , chan = chan
          , msg = message
          , time = time
          }
    stateFile <- (</> "tell.json") <$> getChannelUserState chan target
    exists <- liftIO (doesFileExist stateFile)
    entries <- if exists then
      liftIO (eitherDecodeFileStrict stateFile) >>= \case
        Left err -> do
          liftIO $ putStrLn $ "error decoding " ++ stateFile ++ ": " ++ show err
          return []
        Right entries -> return entries
    else return []
    liftIO $ encodeFile stateFile $ entry : entries
    reply $ nick ++ ": I'll pass that on to " ++ target

tellSnooper :: Plugin
tellSnooper = Plugin
  { pluginName = "commands"
  , pluginCatcher = \_ -> Catched False ()
  , pluginHandler = \_ -> getChannel >>= \case
      Nothing -> return ()
      Just chan -> do
        user <- getUser
        stateFile <- (</> "tell.json") <$> getChannelUserState chan user
        exists <- liftIO (doesFileExist stateFile)
        entries <- if exists then
            liftIO (eitherDecodeFileStrict stateFile) >>= \case
              Left err -> do
                liftIO $ putStrLn $ "error decoding " ++ stateFile ++ ": " ++ show err
                return []
              Right entries -> do
                liftIO $ removeFile stateFile
                return entries
          else return []
        time <- liftIO getCurrentTime
        forM_ entries $ \entry -> do
          reply $ user ++ ": " ++ formatEntry time entry
        return ()
  }
