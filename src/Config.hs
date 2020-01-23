{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Config ( getConfig
              , amqpOptions
              , Config(..)
              , PluginConfig(..)
              , PrConfig(..)
              , enablePr
              , CommandsConfig(..)
              , enableCommands
              , NixreplConfig(..)
              , enableNixrepl
              , LeakedConfig(..)
              , enableLeaked
              , KarmaConfig(..)
              , enableKarma
              , UnregConfig(..)
              , enableUnreg
              , QuitConfig(..)
              , enableQuit
              , pluginConfigForSender
              ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (toLower)
import           Data.List                  (stripPrefix)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
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
  { configEnable                :: Bool
  , configIgnoreStandaloneUnder :: Int
  , configDefaultRepo           :: Text
  , configDefaultOwners         :: Map Text Text
  , configFallbackOwner         :: Text
  } deriving (Show, Generic)

instance FromJSON PrConfig where
  parseJSON = genericParseJSON customOptions

enablePr :: PluginConfig -> Bool
enablePr PluginConfig { configPr = PrConfig { configEnable } } = configEnable

newtype CommandsConfig = CommandsConfig
  { configEnable :: Bool
  } deriving (Show, Generic)

instance FromJSON CommandsConfig where
  parseJSON = genericParseJSON customOptions

enableCommands :: PluginConfig -> Bool
enableCommands PluginConfig { configCommands = CommandsConfig { configEnable } } = configEnable

data NixreplConfig = NixreplConfig
  { configEnable  :: Bool
  , configNixPath :: [String]
  } deriving (Show, Generic)

instance FromJSON NixreplConfig where
  parseJSON = genericParseJSON customOptions

enableNixrepl :: PluginConfig -> Bool
enableNixrepl PluginConfig { configNixrepl = NixreplConfig { configEnable } } = configEnable

newtype LeakedConfig = LeakedConfig
  { configEnable :: Bool
  } deriving (Show, Generic)

instance FromJSON LeakedConfig where
  parseJSON = genericParseJSON customOptions

enableLeaked :: PluginConfig -> Bool
enableLeaked PluginConfig { configLeaked = LeakedConfig { configEnable } } = configEnable

data KarmaConfig = KarmaConfig
  { configEnable    :: Bool
  , configBlacklist :: [Text]
  } deriving (Show, Generic)

instance FromJSON KarmaConfig where
  parseJSON = genericParseJSON customOptions

enableKarma :: PluginConfig -> Bool
enableKarma PluginConfig { configKarma = KarmaConfig { configEnable } } = configEnable

newtype UnregConfig = UnregConfig
  { configEnable :: Bool
  } deriving (Show, Generic)

instance FromJSON UnregConfig where
  parseJSON = genericParseJSON customOptions

enableUnreg :: PluginConfig -> Bool
enableUnreg PluginConfig { configUnreg = UnregConfig { configEnable } } = configEnable

newtype QuitConfig = QuitConfig
  { configEnable :: Bool
  } deriving (Show, Generic)

instance FromJSON QuitConfig where
  parseJSON = genericParseJSON customOptions

enableQuit :: PluginConfig -> Bool
enableQuit PluginConfig { configQuit = QuitConfig { configEnable } } = configEnable

data PluginConfig = PluginConfig
  { configPr       :: PrConfig
  , configCommands :: CommandsConfig
  , configNixrepl  :: NixreplConfig
  , configLeaked   :: LeakedConfig
  , configKarma    :: KarmaConfig
  , configUnreg    :: UnregConfig
  , configQuit     :: QuitConfig
  } deriving (Show, Generic)

instance FromJSON PluginConfig where
  parseJSON = genericParseJSON customOptions


data Config = Config
  { configUser            :: Text
  , configPassword        :: Text
  , configStateDir        :: FilePath
  , configDebugMode       :: Bool
  , configChannelDefaults :: PluginConfig
  , configUsers           :: PluginConfig
  , configChannels        :: Map Text PluginConfig
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions

pluginConfigForSender :: Either Text (Text, Text) -> Config -> PluginConfig
pluginConfigForSender (Left _) = configUsers
pluginConfigForSender (Right (chan, _)) = pluginConfigForChannel chan
  where pluginConfigForChannel channel Config { configChannels, configChannelDefaults } =
          Map.findWithDefault configChannelDefaults channel configChannels

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
