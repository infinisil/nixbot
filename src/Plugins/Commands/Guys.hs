{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugins.Commands.Guys where

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

data GuysCommand
  = GuysHelp
  | GuysRequest Channel User
  deriving Show

guysParser :: Parser GuysCommand
guysParser = try (GuysRequest
                  <$> (char '#' *> fmap T.pack (someTill anySingle space1))
                  <*> (fmap T.pack (some (anySingleBut ' ')) <* eof))
  <|> return GuysHelp

timeInterval :: NominalDiffTime
timeInterval = 60 * 30

maxCount :: Int
maxCount = 1

text :: Channel -> Text
text channel = "Hello and welcome to #" <> channel <> " 👋. We'd appreciate when you address the whole channel using all inclusive words such as; everyone, all, folks, y'all, youz, or fellow humans. Thank you and enjoy your stay! <This is an anonymously sent, pre-written message. If you have any questions, feel free to ask in #nix-diversity>"

helpText :: Text
helpText = ",guys #<channel> <user>: Anonymously send a PM to a user saying \"" <> text "<channel>" <> "\""

validateNonSpam :: Channel -> User -> PluginT App (Either Text ())
validateNonSpam channel target = do
  now <- liftIO getCurrentTime
  path <- (</> "guys-target") <$> getChannelUserState channel target
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
  requesterPath <- (</> "guys-requester") <$> getChannelUserState channel requester
  exists <- liftIO (doesFileExist requesterPath)
  let req = (now, target)
  if exists
    then liftIO (eitherDecodeFileStrict requesterPath) >>= \case
      Left err -> liftIO $ putStrLn $ requesterPath ++ " couldn't be decoded: " ++ show err
      Right (reqs :: [(UTCTime, User)]) ->
        liftIO $ encodeFile requesterPath (req : reqs)
    else liftIO $ encodeFile requesterPath [req]

  targetPath <- (</> "guys-target") <$> getChannelUserState channel target
  exists' <- liftIO (doesFileExist targetPath)
  if exists'
    then liftIO (eitherDecodeFileStrict targetPath) >>= \case
      Left err -> liftIO $ putStrLn $ targetPath ++ " couldn't be decoded: " ++ show err
      Right (reqs :: [UTCTime]) ->
        liftIO $ encodeFile targetPath (now : reqs)
    else liftIO $ encodeFile targetPath [now]

  privMsg target $ text channel
  privMsg requester "The user was informed"


guysHandle :: GuysCommand -> PluginT App ()
guysHandle cmd = do
  user <- getUser
  mchan <- getChannel
  case (mchan, cmd) of
    (Just chan, GuysHelp) -> chanMsg chan $ "Can only be used in PMs: " <> helpText
    (Nothing, GuysHelp) -> privMsg user helpText
    (Just _, GuysRequest _ _) -> privMsg user "The ,guys command only works in PMs"
    (Nothing, GuysRequest chan target) -> do
      spam <- validateNonSpam chan target
      knownUser <- validateUserExists target
      case spam >> knownUser of
        Left err -> privMsg user err
        Right _  -> fulfilRequest user chan target
