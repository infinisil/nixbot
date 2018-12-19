module Types where

import           Config
import           Network.AMQP

data Env = Env
  { config      :: Config
  , amqpChannel :: Channel
  }
