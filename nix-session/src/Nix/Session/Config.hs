{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Session.Config where

import           Data.Map             (Map)
import           System.Process.Typed

import           Data.ByteString.Lazy (fromStrict)
import           Data.FileEmbed       (embedFile)

import           GHC.Generics

import           Data.Aeson           (FromJSON, decode')
{-
Plan for config:
- Both nixbot and nix-session require a config
- Because nixbot uses nix-session, nix-session should be fully configurable from nixbot
- Want to use NixOS module for nixbot config
- Want to use dhall for nix-session config
- NixOS module for nix-session would be annoying: Would have to eval nix everytime at the start
- nixbot should also be configurable with dhall
- NixOS module for nixbot should have all the type equivalents to the dhall config

What to configure for nix-session:
- What to call "self"
- Fixed Nix definitions
- Session location
- External sessions
- readonly mode
- NIX_PATH
- extra Nix options
-}

data Config = Config
  { selfName  :: String
  , fixedDefs :: Map String String
  } deriving (Generic, Show)

instance FromJSON Config

evalConfig :: FilePath -> String -> IO Config
evalConfig nixInstantiate config = do
  y <- readProcessStdout_ process
  case decode' y of
    Nothing -> error "Couldn't decode json value, Nix module is inconsistent with internal representation"
    Just res -> return res
  where
    stdin = byteStringInput $ fromStrict $(embedFile "options.nix")
    args =
      [ "--eval", "--strict", "--json"
      , "--arg", "config", config, "-" ]
    process = setStdin stdin $ proc nixInstantiate args
