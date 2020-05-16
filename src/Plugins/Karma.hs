{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.Karma (karmaPlugin) where

import           Frontend.Types
import           Plugins
import           Log

import           Config
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Maybe
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import Control.Concurrent (threadDelay)
import           Data.Time
import           GHC.Generics
import           IRC
import           System.Directory
import           System.FilePath
import           Text.Regex.TDFA      ((=~))
import           Types
import Numeric
import Data.Char (intToDigit)
import System.Random


karmaRegex :: String
karmaRegex = "([^[:space:]]+)\\+\\+|<3 ([^[:space:]]+)|✨ ([^[:space:]]+)"

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

rateLimited :: Channel -> User -> PluginT App Bool
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
  when tooMany $ chanMsg channel $ user <> ": You've been giving a bit too much karma lately!"
  return tooMany

matchFilter :: [Text] -> PluginT App [Text]
matchFilter matches = do
  sender <- getSender
  pluginConfig <- lift $ asks (pluginConfigForSender sender . config)
  let blacklist = Set.fromList $ configBlacklist $ configKarma pluginConfig
      matchSet = Set.fromList matches
      filtered = matchSet `Set.difference` blacklist
  return $ Set.toList filtered

type KarmaAction = (Int -> Bool, Int -> Channel -> User -> PluginT App () -> PluginT App ())

increaseMessages :: [(Double, KarmaAction)]
increaseMessages =
  [ (100, (const True, \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to " <> show' (n + 1))))
  , (3, (const True, \n chan nick increase -> increase *> increase *> chanMsg chan (nick <> "'s karma got increased to " <> show' (n + 2) <> ", it's a crit!")))
  , (2, (const True, \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to like " <> show' (n + 1) <> ", I think")))
  , (2, (const True, \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to " <> show' (n + 1) <> ", that's Numberwang!")))
  , (1, (const True, \_ chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased!")))
  , (1, (const True, \n chan nick increase -> do
            increase
            chanMsg chan (nick <> "'s karma got decreased to " <> show' (n - 1))
            liftIO $ threadDelay 10000000
            chanMsg chan ("Wait no, it got *increased* to " <> show' (n + 1))
    ))
  , (1, (const True, \_ chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to -2147483648")))
  , (4, ( \n -> floatIncrease (fromIntegral n) /= fromIntegral (n + 1)
            , \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to " <> show' (floatIncrease (fromIntegral n)))))
  , (100, (\n -> length (show (n + 1)) > length (show n), \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma now has " <> show' (length (show (n + 1))) <> " digits!")))
  , (2, ((>= 0), \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to 0b" <> Text.pack (showIntAtBase 2 intToDigit (n + 1) ""))))
  , (2, ((>= 0), \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to 0o" <> Text.pack (showOct (n + 1) ""))))
  , (2, ((>= 0), \n chan nick increase -> increase *> chanMsg chan (nick <> "'s karma got increased to 0x" <> Text.pack (showHex (n + 1) ""))))
  , (1, ((>= 0), \_ chan nick increase -> increase *> chanMsg chan (nick <> " was put on Santa's \"nice\" list")))
  ] where
  floatIncrease :: Double -> Double
  floatIncrease n = n ** logBase n (n + 1)

selectWithFrequency :: Double -> [(Double, a)] -> a
selectWithFrequency randomValue items = selectedItem
  where
    total = sum (map fst items)
    selectedValue = total * randomValue
    selectedIndex = length (filter (<=selectedValue) $ scanl (+) 0 $ map fst items) - 1
    selectedItem = snd $ items !! selectedIndex
    

triggerIncrease :: Int -> Channel -> User -> PluginT App () -> PluginT App ()
triggerIncrease oldAmount chan nick increase = do
  randomValue :: Double <- liftIO randomIO
  let potentialMessages = filter (\(_, (f, _)) -> f oldAmount) increaseMessages
      selected = snd $ selectWithFrequency randomValue potentialMessages
  selected oldAmount chan nick increase

karmaPlugin :: Plugin
karmaPlugin = Plugin
  { pluginName = "karma"
  , pluginCatcher = \input@Input { inputMessage } ->
      case filter (not . null) . concatMap tail $ (Text.unpack inputMessage =~ karmaRegex :: [[String]]) of
        []      -> PassedOn
        matches -> Catched True (input, map Text.pack matches)
  , pluginHandler = \(Input { inputSender, inputMessage }, unfilteredMatches) -> do
      matches <- matchFilter unfilteredMatches
      case inputSender of
        Left user -> privMsg user $ "As much as you love "
          <> Text.intercalate ", " matches <> ", you can't give them karma here!"
        Right (channel, user) -> rateLimited channel user >>= \limited -> unless limited $ do
          time <- liftIO getCurrentTime

          forM_ matches $ \receiver ->
            lift (isKnown receiver) >>= \case
              False -> return ()
              True -> do
                receiverFile <- (</> "entries") <$> getUserState receiver
                exists <- liftIO $ doesFileExist receiverFile
                entries <- if not exists then return [] else
                  liftIO (eitherDecodeFileStrict receiverFile) >>= \case
                    Left err -> do
                      liftIO $ putStrLn $ "Couldn't decode karma file: " ++ err
                      return []
                    Right entries -> return entries
                if receiver == user then do
                  let entry = SelfKarma
                        { givenIn = channel
                        , givenAt = time
                        , karmaContext = inputMessage
                        }
                  let newEntries = entry : entries
                  liftIO $ encodeFile receiverFile newEntries
                  chanMsg channel $ receiver <> "'s karma got decreased to " <> show' (countKarma newEntries)
                else do
                  let entry = KarmaEntry
                        { givenBy = user
                        , givenIn = channel
                        , givenAt = time
                        , karmaContext = inputMessage
                        }
                  let oldKarma = countKarma entries
                  triggerIncrease oldKarma channel receiver $ do
                    exists' <- liftIO $ doesFileExist receiverFile
                    oldEntries <- if not exists' then return [] else
                      liftIO (eitherDecodeFileStrict receiverFile) >>= \case
                        Left err -> do
                          liftIO $ putStrLn $ "Couldn't decode karma file: " ++ err
                          return []
                        Right entries' -> return entries'
                    let newEntries = entry : oldEntries
                    liftIO $ encodeFile receiverFile newEntries
  }
