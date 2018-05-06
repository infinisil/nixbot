{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config ( getConfig
              , amqpOptions
              , Config(..)
              ) where

import           NixEval
import           Utils.FileLiteral

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Map                   (singleton)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Network.AMQP
import           System.Environment         (getArgs)

data Config = Config
  { user     :: Text
  , password :: Text
  , stateDir :: FilePath
  , argsPath :: FilePath
  } deriving (Show, Generic)

instance FromJSON Config where

amqpOptions :: Config -> ConnectionOpts
amqpOptions Config { user, password } = defaultConnectionOpts
  { coVHost = "ircbot"
  , coTLSSettings = Just TLSTrusted
  , coServers = [("events.nix.gsc.io", 5671)]
  , coAuth = [ amqplain user password ]
  }

getConfig :: IO Config
getConfig = do
  configFile:_ <- getArgs
  result <- nixInstantiate def
    { contents = [litFile|options.nix|]
    , arguments = singleton "cfg" ("import " ++ configFile)
    , nixPath = ["nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz"]
    , mode = Json
    }
  case result of
    Left error -> fail $ "Error evaluating config file: " ++ show error
    Right result -> case eitherDecode $ pack result of
      Left error   -> fail $ "Error reading json value: " ++ show error
      Right config -> return config
