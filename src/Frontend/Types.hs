{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Types
  ( shapeInput
  , Input(..)
  , inputUser
  , inputChannel
  , Output(..)
  , shapeOutput
  , Frontend(..)
  ) where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Data.Aeson
import           Data.List                      (stripPrefix)
import           Data.Maybe                     (fromJust)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           GHC.Generics                   (Generic)
import           IRC
import qualified Network.AMQP                   as A

data Frontend = Frontend
  { outputQueue :: TMQueue Output
  , amqpChannel :: TMVar A.Channel
  }

-- Fields in https://github.com/grahamc/ircbot/blob/master/ircbot/src/bin/gateway.rs

data RawInput = RawInput
  { in_from   :: Text
  , in_body   :: Text
  , in_sender :: Text
  } deriving (Show, Generic)

data RawOutput = RawOutput
  { out_target       :: Text
  , out_body         :: Text
  , out_message_type :: Text
  } deriving (Show, Generic)

instance FromJSON RawInput where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJust . stripPrefix "in_" }
instance ToJSON RawOutput where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = fromJust . stripPrefix "out_" }

data Input = Input
  { inputSender  :: Either User (Channel, User)
  , inputMessage :: Message
  } deriving (Show)

inputUser :: Input -> User
inputUser Input { inputSender = Left user }       = user
inputUser Input { inputSender = Right (_, user) } = user

inputChannel :: Input -> Maybe Channel
inputChannel Input { inputSender = Left _ }          = Nothing
inputChannel Input { inputSender = Right (chan, _) } = Just chan

data Output = Output
  { outputReceiver :: Either User Channel
  , outputMessage  :: Message
  } deriving Show


shapeInput :: RawInput -> Input
shapeInput RawInput { in_from, in_body, in_sender } = Input
  { inputMessage = in_body
  , inputSender = case Text.uncons in_from of
      Just ('#', chan) -> Right (chan, in_sender)
      _                -> Left in_sender
  }


shapeOutput :: Output -> RawOutput
shapeOutput Output { outputReceiver, outputMessage } = RawOutput
  { out_body = outputMessage
  , out_target = case outputReceiver of
      Left user     -> user
      Right channel -> '#' `Text.cons` channel
  , out_message_type = "privmsg"
  }

