{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Config
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           GHC.Generics
import           IRC
import qualified Network.AMQP

data SharedState = SharedState
  { knownUsers :: Set User
  } deriving (Show, Generic)

instance FromJSON SharedState
instance ToJSON SharedState

data Env = Env
  { config      :: Config
  , amqpChannel :: Network.AMQP.Channel
  , sharedState :: TVar SharedState
  }
