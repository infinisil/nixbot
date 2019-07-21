{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugins.Commands.Expand where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Set                as Set
import           Data.Text               as T
import           Data.Time
import           IRC
import           Plugins
import           Plugins.Commands.Shared
import           System.Directory
import           System.FilePath
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Types

data ExpandCommand
  = ExpandHelp
  | ExpandRequest Channel User
  deriving Show

expandParser :: Parser ExpandCommand
expandParser = (try (ExpandRequest <$> (char '#' *> fmap T.pack (someTill anySingle space1)) <*> (fmap T.pack (some (anySingleBut ' ')) <* eof)) <|> return ExpandHelp)

timeInterval :: NominalDiffTime
timeInterval = 60 * 30 -- 60 * 60

maxCount :: Int
maxCount = 1

text :: Text
text = "Please expand your question to include more information, this will help us help you :)"

helpText :: Text
helpText = ",expand #<channel> <user>: Anonymously send \"" <> text <> "\" to a user in a specific channel"

validateNonSpam :: Channel -> User -> PluginT App (Either Text ())
validateNonSpam channel target = do
  now <- liftIO getCurrentTime
  path <- (</> "expand-target") <$> getChannelUserState channel target
  exists <- liftIO (doesFileExist path)
  if exists
    then liftIO (eitherDecodeFileStrict path) >>= \case
      Left err -> do
        liftIO $ putStrLn $ path ++ " couldn't be decoded: " ++ show err
        return (Left "internal decoding error")
      Right (reqs :: [UTCTime]) -> do
        let recentReqCount =
              Prelude.length
              . Prelude.filter (\time -> now `diffUTCTime` time < timeInterval)
              $ reqs
        return $ if recentReqCount >= maxCount
          then Left "This user was already informed recently"
          else Right ()
    else return (Right ())

validateUserExists :: User -> PluginT App (Either Text ())
validateUserExists target = do
  stateVar <- lift $ asks sharedState
  state <- liftIO $ readTVarIO stateVar
  return $ if Set.member target (knownUsers state)
    then Right ()
    else Left "No such user is known"

fulfilRequest :: User -> Channel -> User -> PluginT App ()
fulfilRequest requester channel target = do
  now <- liftIO getCurrentTime
  requesterPath <- (</> "expand-requester") <$> getChannelUserState channel requester
  exists <- liftIO (doesFileExist requesterPath)
  let req = (now, target)
  if exists
    then liftIO (eitherDecodeFileStrict requesterPath) >>= \case
      Left err -> liftIO $ putStrLn $ requesterPath ++ " couldn't be decoded: " ++ show err
      Right (reqs :: [(UTCTime, User)]) ->
        liftIO $ encodeFile requesterPath (req : reqs)
    else liftIO $ encodeFile requesterPath [req]

  targetPath <- (</> "expand-target") <$> getChannelUserState channel target
  exists' <- liftIO (doesFileExist targetPath)
  if exists'
    then liftIO (eitherDecodeFileStrict targetPath) >>= \case
      Left err -> liftIO $ putStrLn $ targetPath ++ " couldn't be decoded: " ++ show err
      Right (reqs :: [UTCTime]) ->
        liftIO $ encodeFile targetPath (now : reqs)
    else liftIO $ encodeFile targetPath [now]

  chanMsg channel $ target <> ": " <> text
  privMsg requester "The user was informed"


expandHandle :: ExpandCommand -> PluginT App ()
expandHandle cmd = do
  user <- getUser
  mchan <- getChannel
  case (mchan, cmd) of
    (Just chan, ExpandHelp) -> chanMsg chan $ helpText <> " (only works in PMs)"
    (Nothing, ExpandHelp) -> privMsg user helpText
    (Just _, ExpandRequest _ _) -> privMsg user "The ,expand command only works in PMs"
    (Nothing, ExpandRequest chan target) -> do
      spam <- validateNonSpam chan target
      knownUser <- validateUserExists target
      case spam >> knownUser of
        Left err -> privMsg user err
        Right _  -> fulfilRequest user chan target
