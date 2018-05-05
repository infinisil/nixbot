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
import           Network.AMQP          (SASLMechanism, amqplain)

import           Text.Toml             (parseTomlDoc)
import           Text.Toml.Types       (Node (..))


data Options =
  Options
    { name      :: Text
    , password  :: Text
    , stateDir' :: FilePath
    }
  | ConfigFile FilePath


data Config = Config
  { auth     :: SASLMechanism
  , stateDir :: FilePath
  }

getConfig :: IO Config
getConfig = do
  result <- execParser opts
  case result of
    Options { name, password, stateDir' } -> return Config
      { auth = amqplain name password
      , stateDir = stateDir'
      }
    ConfigFile path -> do
      contents <- readFile path
      let toml = parseTomlDoc path (pack contents)
      case toml of
        Left error -> fail $
          "Error parsing config file: " ++ parseErrorPretty error
        Right value -> case (H.lookup "name" value, H.lookup "password" value) of
          (Just (VString name), Just (VString password)) -> case H.lookup "stateDir" value of
            Just (VString stateDir') -> return Config
              { auth = amqplain name password
              , stateDir = unpack stateDir'
              }
            _ -> fail "config file couldn't parse a stateDir field"
          _ -> fail "config file couldn't parse a name and/or password field"



options :: Parser Options
options = Options
  <$> strOption
    ( long "name"
    <> metavar "NAME"
    <> help "Username to use for connecting to the amqp server" )
  <*> strOption
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
