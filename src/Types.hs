{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Config
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Set                       (Set)
import           Data.Text                      (Text)
import           Frontend.Types
import           GHC.Generics
import           IRC

newtype SharedState = SharedState
  { knownUsers :: Set User
  } deriving (Show, Generic)

instance FromJSON SharedState
instance ToJSON SharedState

data Env = Env
  { config      :: Config
  , logQueue    :: TMQueue Text
  , sharedState :: TVar SharedState
  , frontend    :: Frontend
  }

type App = ReaderT Env IO
