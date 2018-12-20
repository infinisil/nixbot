{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.Karma (karmaPlugin) where

import           Plugins

import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString     as BS
import           Data.Either
import           Data.List           (intercalate, nub)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes, listToMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Time
import           GHC.Generics
import           IRC
import           System.Directory
import           System.FilePath
import           Text.Regex.TDFA     ((=~))


karmaRegex :: String
karmaRegex = "([^[:space:]]+)\\+\\+"

blacklistedUsers = Set.fromList [ "c", "C", "g", "avr-g" ]

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
countKarma []                   = 0
countKarma (KarmaEntry {}:rest) = countKarma rest + 1
countKarma (SelfKarma {}:rest)  = countKarma rest - 1

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
  rates <- if not exists then return [] else do
    liftIO (decodeFileStrict rateFile) >>= \case
      Nothing                   -> do
        liftIO $ putStrLn "rates couldn't be decoded"
        return []
      Just (times :: [UTCTime]) -> return times
  let onlyRecent = takeWhile ((< snd maxKarmaPerTime) . diffUTCTime time) rates
  let tooMany = fst maxKarmaPerTime <= length onlyRecent
  liftIO $ encodeFile rateFile $ time : onlyRecent
  if tooMany then do
    chanMsg channel $ user ++ ": You've been giving a bit too much karma lately!"
    return False
  else return True

karmaPlugin :: Plugin
karmaPlugin = Plugin
  { pluginName = "karma"
  , pluginCatcher = \input@Input { inputMessage } ->
      case fmap (!!1) (inputMessage =~ karmaRegex :: [[String]]) of
        []      -> PassedOn
        matches -> Consumed (input, nub matches)
  , pluginHandler = \(Input { inputChannel, inputUser, inputMessage }, matches) -> do
      case inputChannel of
        Nothing -> privMsg inputUser $ "As much as you love "
          ++ intercalate ", " matches ++ ", you can't give them karma here!"
        Just channel -> rateLimited channel inputUser >>= \case
          False -> return ()
          True -> do
            time <- liftIO getCurrentTime

            results <- forM matches $ \receiver ->
              isKnown receiver >>= \case
                False -> return $ Left receiver
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
                  entries <- if not exists then return [] else do
                    liftIO (decodeFileStrict receiverFile) >>= \case
                      Nothing -> do
                        liftIO $ putStrLn "Couldn't decode karma file"
                        return []
                      Just entries -> return entries
                  let newEntries = entry : entries
                  liftIO $ encodeFile receiverFile newEntries
                  return $ Right $ receiver ++ "'s karma got "
                    ++ if selfKarma then "decreased" else "increased"
                    ++ " to " ++ show (countKarma newEntries)

            let (unknown, known) = partitionEithers results

            chanMsg channel $ intercalate ", " known
            --chanMsg channel $ "Can't give karma to " ++ intercalate ", " unknown ++ " because they haven't been sighted"
            return ()
  }
