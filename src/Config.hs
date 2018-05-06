{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config ( getConfig
              , Config(..)
              ) where

import           NixEval
import           Utils.FileLiteral

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Aeson
import qualified Data.HashMap.Strict        as H
import qualified Data.Map                   as M
import           Data.Semigroup             ((<>))
import           GHC.Generics
import           Options.Applicative
import           System.Environment         (getArgs)
import           Text.Megaparsec.Error

import           Data.Text                  (Text, pack, unpack)
import           Network.AMQP

import           Text.Toml                  (parseTomlDoc)
import           Text.Toml.Types            (Node (..))

import           Network.Socket             (PortNumber)


data NewOptions = NewOptions
  { name     :: Text
  , password :: Text
  , stateDir :: FilePath
  } deriving (Show, Generic)

instance FromJSON NewOptions where

data Config = Config
  { amqpOptions :: ConnectionOpts
  , stateDir'   :: FilePath
  }

makeConfig :: NewOptions -> Config
makeConfig NewOptions { name, password, stateDir } = Config
  { amqpOptions = defaultConnectionOpts
    { coVHost = "ircbot"
    , coTLSSettings = Just TLSTrusted
    , coServers = [("events.nix.gsc.io", 5671)]
    , coAuth = [ amqplain "ircbot-infinisil" password ]
    }
  , stateDir' = stateDir
  }

options = [litFile|options.nix|]

getConfig :: IO Config
getConfig = do
  configFile:_ <- getArgs
  result <- nixInstantiate options Nothing (M.singleton "cfg" ("import " ++ configFile)) ["nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz"] Json def
  case result of
    Left error -> fail $ "Error evaluating config file: " ++ show error
    Right result -> case eitherDecode $ B.pack result of
      Left error   -> fail $ "Error reading json value: " ++ show error
      Right config -> return $ makeConfig config
