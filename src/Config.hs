{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Config ( getConfig
              , amqpOptions
              , Config(..)
              , PluginConfig(..)
              , PrConfig(..)
              ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (toLower)
import           Data.List                  (stripPrefix)
import           Data.Map                   (Map)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Network.AMQP
import           Options.Applicative
import           System.Directory           (makeAbsolute)

parser :: Parser FilePath
parser = argument str
         ( metavar "FILE"
        <> help "Config file" )

opts :: ParserInfo FilePath
opts = info (parser <**> helper)
   ( fullDesc
  <> progDesc "Nixbot"
  <> header "nixbot - nix bot"
   )

lowerFirst :: String -> String
lowerFirst []       = []
lowerFirst (c:rest) = toLower c : rest


customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = \field ->
      lowerFirst .
      fromMaybe (error $ "field " ++ field ++ " in Config.hs not prefixed with \"config\"") .
      stripPrefix "config" $
      field
  }

data PrConfig = PrConfig
  { configIgnoreStandaloneUnder :: Int
  , configDefaultRepo           :: Text
  , configDefaultOwners         :: Map Text Text
  , configFallbackOwner         :: Text
  } deriving (Show, Generic)

instance FromJSON PrConfig where
  parseJSON = genericParseJSON customOptions

newtype PluginConfig = PluginConfig
  { configPr :: PrConfig
  } deriving (Show, Generic)

instance FromJSON PluginConfig where
  parseJSON = genericParseJSON customOptions

data Config = Config
  { configUser           :: Text
  , configPassword       :: Text
  , configStateDir       :: FilePath
  , configNixPath'       :: [String]
  , configKarmaBlacklist :: [Text]
  , configDebugMode      :: Bool
  , configPlugins        :: PluginConfig
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions



amqpOptions :: Config -> ConnectionOpts
amqpOptions Config { configUser, configPassword } = defaultConnectionOpts
  { coVHost = "ircbot"
  , coTLSSettings = Just TLSTrusted
  , coServers = [("events.nix.gsc.io", 5671)]
  , coAuth = [ amqplain configUser configPassword ]
  }

getConfig :: IO Config
getConfig = do
  configFile <- execParser opts >>= makeAbsolute >>= BS.readFile

  either fail return $ eitherDecode' configFile
