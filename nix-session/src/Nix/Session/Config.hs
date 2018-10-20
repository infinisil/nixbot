{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Session.Config where

import           Control.Lens         (makeLenses)
import           Data.Aeson           (FromJSON (..), defaultOptions,
                                       eitherDecode', fieldLabelModifier,
                                       genericParseJSON)
import           Data.Map             (Map)
import           GHC.Generics         (Generic)
import           Paths_nix_session    (getDataFileName)
import           System.Process.Typed (proc, readProcessStdout_)

{-
Plan for config:
- Both nixbot and nix-session require a config
- Because nixbot uses nix-session, nix-session should be fully configurable from nixbot
- Want to use NixOS module for nixbot and nix-session config

What to configure for nix-session:
- What to call "self"
- Fixed Nix definitions
- Session location
- External sessions
- readonly mode
- NIX_PATH
- extra Nix options

*Actually*:
- There's session-wide config, which should be changeable when creating a new session or with special commands during one. This includes selfName, fixedDefs, NIX_PATH, nix options
- Command config, such as session location, external sessions, readonly mode, defaults for session-wide configs too

- Should nixPath and options be part of the session config? Because evaluating things from secondary sessions can't honor these.
-}

data SessionConfig = SessionConfig
  { _selfName  :: String
  , _metaName  :: String
  , _fixedDefs :: Map String String
  } deriving (Show, Generic)

data Config = GlobalConfig
  { _primarySession    :: FilePath
  , _secondarySessions :: Map String FilePath
  , _sessionDefaults   :: SessionConfig
  , _nixPath           :: Maybe [String]
  , _nixOptions        :: Map String String
  } deriving (Show, Generic)

makeLenses ''SessionConfig
makeLenses ''Config

lensOptions = defaultOptions { fieldLabelModifier = tail }

instance FromJSON SessionConfig where
  parseJSON = genericParseJSON lensOptions

instance FromJSON Config where
  parseJSON = genericParseJSON lensOptions

evalConfig :: FilePath -> FilePath -> IO Config
evalConfig nixInstantiate config = do
  nixFile <- getDataFileName "nix/default.nix"
  let args = [ "--eval", "--strict", "--json"
             , "--arg", "config", config, nixFile ]
  result <- readProcessStdout_ $ proc nixInstantiate args
  case eitherDecode' result of
    Left err -> fail $ "Couldn't decode json value, Nix module is inconsistent with internal representation: " ++ err
    Right res -> return res
