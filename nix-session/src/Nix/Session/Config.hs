{-# LANGUAGE DeriveGeneric #-}

module Nix.Session.Config where

import           Control.Lens               (makeLenses)
import           Data.Aeson                 (FromJSON (..), defaultOptions,
                                             eitherDecode', fieldLabelModifier,
                                             genericParseJSON)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   (Map)
import           Data.SafeCopy
import           Data.Text                  (Text, pack)
import           GHC.Generics               (Generic)
import           Nix.Session.Types
import           Paths_nix_session          (getDataFileName)
import           System.Exit
import           System.Process.Typed       (proc, readProcess)

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


evalConfig :: FilePath -> FilePath -> IO GlobalConfig
evalConfig nixInstantiate config = do
  nixFile <- getDataFileName "nix/default.nix"
  let args = [ "--eval", "--strict", "--json"
             , "--arg", "config", config, nixFile ]
  (code, stdout, stderr) <- readProcess $ proc nixInstantiate args
  case code of
    ExitSuccess -> case eitherDecode' stdout of
      Left err -> fail $ "Couldn't decode json value, Nix module is inconsistent with internal representation: " ++ err
      Right res -> return res
    ExitFailure _ -> fail $ BS.unpack stderr
