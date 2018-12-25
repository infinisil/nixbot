{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Config ( getConfig
              , amqpOptions
              , Config(..)
              ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
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

data Config = Config
  { user     :: Text
  , password :: Text
  , stateDir :: FilePath
  , nixPath' :: [String]
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
  configFile <- execParser opts >>= makeAbsolute >>= BS.readFile

  either fail return $ eitherDecode' configFile
