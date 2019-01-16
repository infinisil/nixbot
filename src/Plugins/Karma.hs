{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.Karma (karmaPlugin) where

import           Plugins

import           Config
import           Control.Monad.Reader
import           Data.Aeson
import           Data.List            (intercalate)
import           Data.Maybe
import qualified Data.Set             as Set
import           Data.Time
import           GHC.Generics
import           IRC
import           System.Directory
import           System.FilePath
import           Text.Regex.TDFA      ((=~))


karmaRegex :: String
karmaRegex = "([^[:space:]]+)\\+\\+"

-- TODO: Use a single constructor with givenBy being a Maybe (or some iso of it)
-- Needs to migrate the old data
data KarmaEntry
  = KarmaEntry
  { givenBy      :: User
  , givenIn      :: Channel
  , givenAt      :: UTCTime
  , karmaContext :: Message
  }
  | SelfKarma
  { givenIn      :: Channel
  , givenAt      :: UTCTime
  , karmaContext :: Message
  } deriving (Show, Generic)

countKarma :: [KarmaEntry] -> Int
countKarma entries = sum $ map value entries where
  value KarmaEntry {} = 1
  value SelfKarma {}  = -1

instance FromJSON KarmaEntry
instance ToJSON KarmaEntry

maxKarmaPerTime :: (Int, NominalDiffTime)
maxKarmaPerTime = (10, 5*60)

rateLimited :: (PluginMonad m, MonadIO m, IRCMonad m) => Channel -> User -> m Bool
rateLimited channel user = do
  giverDir <- getUserState user

  time <- liftIO getCurrentTime
  let rateFile = giverDir </> "rate"
  exists <- liftIO (doesFileExist rateFile)
  rates <- if not exists then return [] else
    liftIO (eitherDecodeFileStrict rateFile) >>= \case
      Left err                   -> do
        liftIO $ putStrLn $ "rates couldn't be decoded: " ++ err
        return []
      Right (times :: [UTCTime]) -> return times
  let onlyRecent = takeWhile ((< snd maxKarmaPerTime) . diffUTCTime time) rates
  let tooMany = fst maxKarmaPerTime <= length onlyRecent
  liftIO $ encodeFile rateFile $ time : onlyRecent
  when tooMany $ chanMsg channel $ user ++ ": You've been giving a bit too much karma lately!"
  return tooMany

matchFilter :: MonadReader Config m => [String] -> m [String]
matchFilter matches = do
  blacklist <- Set.fromList <$> asks karmaBlacklist
  let matchSet = Set.fromList matches
      filtered = matchSet `Set.difference` blacklist
  return $ Set.toList filtered

karmaPlugin :: Plugin
karmaPlugin = Plugin
  { pluginName = "karma"
  , pluginCatcher = \input@Input { inputMessage } ->
      case fmap (!!1) (inputMessage =~ karmaRegex :: [[String]]) of
        []      -> PassedOn
        matches -> Catched True (input, matches)
  , pluginHandler = \(Input { inputChannel, inputUser, inputMessage }, unfilteredMatches) -> do
      matches <- matchFilter unfilteredMatches
      case inputChannel of
        Nothing -> privMsg inputUser $ "As much as you love "
          ++ intercalate ", " matches ++ ", you can't give them karma here!"
        Just channel -> rateLimited channel inputUser >>= \limited -> unless limited $ do
          time <- liftIO getCurrentTime

          results <- forM matches $ \receiver ->
            isKnown receiver >>= \case
              False -> return Nothing
              True -> do
                -- TODO: Only allow karma for users that have talked before
                let selfKarma = receiver == inputUser
                let entry = if selfKarma then SelfKarma
                      { givenIn = channel
                      , givenAt = time
                      , karmaContext = inputMessage
                      }
                    else KarmaEntry
                      { givenBy = inputUser
                      , givenIn = channel
                      , givenAt = time
                      , karmaContext = inputMessage
                      }

                receiverFile <- (</> "entries") <$> getUserState receiver
                exists <- liftIO $ doesFileExist receiverFile
                entries <- if not exists then return [] else
                  liftIO (eitherDecodeFileStrict receiverFile) >>= \case
                    Left err -> do
                      liftIO $ putStrLn $ "Couldn't decode karma file: " ++ err
                      return []
                    Right entries -> return entries
                let newEntries = entry : entries
                liftIO $ encodeFile receiverFile newEntries
                return $ Just $ receiver ++ "'s karma got "
                  ++ (if selfKarma then "decreased" else "increased")
                  ++ " to " ++ show (countKarma newEntries)

          chanMsg channel $ intercalate ", " $ catMaybes results
  }
