{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Config ( getConfig
              , amqpOptions
              , Config(..)
              ) where

import           NixEval

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.FileEmbed             (embedFile)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Network.AMQP
import           Paths_nixbot               (getDataFileName)
import           System.Directory           (findExecutable, makeAbsolute)
import           System.Environment         (getArgs)

import           Options.Applicative

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
  configFile <- execParser opts >>= makeAbsolute
  optionsFile <- getDataFileName "options.nix"
  nixInstPath <- fromMaybe (fail "Couldn't find nix-instantiate executable") <$> findExecutable "nix-instantiate"

  result <- nixInstantiate nixInstPath (defNixEvalOptions $ Right optionsFile)
    { arguments = M.singleton "cfg" configFile
    , mode = Json
    }
  case result of
    Left err  -> fail $ BS.unpack err
    Right out -> either fail return $ eitherDecode' out
