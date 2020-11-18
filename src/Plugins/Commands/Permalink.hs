{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugins.Commands.Permalink where

import           qualified Data.Text               as T
import Data.Maybe
import IRC

import Control.Monad.State
import           Plugins
import           Types
import Data.Text (Text)

import Text.HTML.DOM
import Data.Conduit
import Data.XML.Types
import qualified Data.Conduit.Combinators as C
import Network.HTTP.Simple (httpSink, parseRequest, getResponseStatusCode)

import Data.Conduit.Lift (evalStateC)

getIds :: ConduitT Event Text IO ()
getIds = awaitForever $ \case
  EventBeginElement (Name "div" _ _) contents | any isTalk contents ->
    case listToMaybe $ mapMaybe getId contents of
      Just theId -> yield theId
      _ -> return ()
  _ -> return ()

getDate :: ConduitT Event Text IO ()
getDate = evalStateC False $ awaitForever $ \case
  EventBeginElement (Name "title" _ _) _ -> put True
  EventContent (ContentText txt) -> do
    isTitle <- get
    when isTitle $
      yield $ T.words txt !! 2
  _ -> return ()

isTalk :: (Name, [Content]) -> Bool
isTalk (Name "class" _ _, [ContentText txt])
  | "talk" `elem` T.words txt = True
isTalk _ = False

getId :: (Name, [Content]) -> Maybe Text
getId (Name "id" _ _, [ContentText i]) = Just i
getId _ = Nothing


getDateId :: Channel -> IO (Either String (Text, Text))
getDateId chan = do
  req <- parseRequest $ "https://logs.nix.samueldr.com/" ++ T.unpack chan
  httpSink req $ \resp -> case getResponseStatusCode resp of
    200 -> do
      [mid, mdate] <- eventConduit .| sequenceConduits [ getDate .| C.head, getIds .| C.last ]
      case (mid, mdate) of
        (Just theId, Just date) -> return $ Right (theId, date)
        _ -> return $ Left "Couldn't parse document"
    _ -> return $ Left ("Non-200 response code from " ++ show req)

getLatestChannelMessageUrl :: Channel -> IO (Either String Text)
getLatestChannelMessageUrl chan = do
  did <- getDateId chan
  case did of
    Left err -> return $ Left err
    Right (date, theId) -> return $ Right $ "https://logs.nix.samueldr.com/" <> chan <> "/" <> date <> "#" <> theId

permalinkHandle :: PluginT App ()
permalinkHandle = getChannel >>= \case
  Nothing -> reply ",permalink can only be used in public channels"
  Just chan -> do
    murl <- liftIO $ getLatestChannelMessageUrl chan
    case murl of
      Right url -> reply url
      Left err -> reply $ T.pack $ "Error: " ++ err
