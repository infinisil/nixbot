{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Session.Config (evalConfig, Config(..), selfName, fixedDefs) where

import           Data.Map             (Map)

import           Control.Lens
import           Data.Aeson           (FromJSON, decode', fieldLabelModifier,
                                       genericParseJSON)
import           Data.Aeson.Types
import           Data.ByteString.Lazy (fromStrict)
import           Data.FileEmbed       (embedFile)
import           GHC.Generics

import           System.Directory     (makeAbsolute)
import           System.Process.Typed
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
-}

data Config = Config
  { _selfName  :: String
  , _fixedDefs :: Map String String
  } deriving (Generic, Show)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = tail }


evalConfig :: FilePath -> FilePath -> IO Config
evalConfig nixInstantiate config = do
  y <- readProcessStdout_ process
  case decode' y of
    Nothing -> error "Couldn't decode json value, Nix module is inconsistent with internal representation"
    Just res -> return res

  where
    args = [ "--eval", "--strict", "--json"
           , "--arg", "config", config, "-" ]
    process = setStdin stdin $ proc nixInstantiate args
    stdin = byteStringInput $ fromStrict $(embedFile "options.nix")
