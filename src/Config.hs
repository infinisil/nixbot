{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Config ( getConfig
              , Config(..)
              ) where

import qualified Data.HashMap.Strict   as H
import           Data.Semigroup        ((<>))
import           Options.Applicative
import           Text.Megaparsec.Error

import           Data.Text             (Text, pack, unpack)
import           Network.AMQP

import           Text.Toml             (parseTomlDoc)
import           Text.Toml.Types       (Node (..))

import Network.Socket (PortNumber)


data Options =
  Options
    { password  :: Text
    , stateDir' :: FilePath
    }
  | ConfigFile FilePath

data Config = Config
  { amqpOptions :: ConnectionOpts
  , stateDir    :: FilePath
  }

makeConfig :: Text -> FilePath -> Config
makeConfig password stateDir = Config
  { amqpOptions = defaultConnectionOpts
    { coVHost = "ircbot"
    , coTLSSettings = Just TLSTrusted
    , coServers = [("events.nix.gsc.io", 5671)]
    , coAuth = [ amqplain "ircbot-infinisil" password ]
    }
  , stateDir = stateDir
  }

getConfig :: IO Config
getConfig = do
  result <- execParser opts
  case result of
    Options password stateDir -> return $ makeConfig password stateDir
    ConfigFile path -> do
      contents <- readFile path
      let toml = parseTomlDoc path (pack contents)
      case toml of
        Left error -> fail $
          "Error parsing config file: " ++ parseErrorPretty error
        Right value -> case H.lookup "password" value of
          Just (VString password) -> case H.lookup "stateDir" value of
            Just (VString stateDir') -> return $ makeConfig password (unpack stateDir')
            _ -> fail "config file couldn't get a stateDir field"
          _ -> fail "config file couldn't get a name field"



options :: Parser Options
options = Options
  <$> strOption
    ( long "password"
    <> metavar "PASSWORD"
    <> help "Password to use for connecting to the amqp server" )
  <*> strOption
    ( long "stateDir"
    <> metavar "PATH"
    <> help "Path to save the state" )

configFile :: Parser Options
configFile = ConfigFile
  <$> strOption
    ( long "config"
    <> metavar "FILE"
    <> help "Config to use for passing options" )

opts :: ParserInfo Options
opts = info (configFile <|> options <**> helper)
  ( fullDesc
  <> progDesc "Run infinisil's nixbot"
  <> header "nixbot" )
