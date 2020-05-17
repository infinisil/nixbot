{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugins.Commands.Escape where

import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Plugins
import           Types
import           NixEval
import Data.Char
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Control.Monad.IO.Class

data Part
  = Literal Char
  | Escaped Char
  deriving (Show, Eq)

type NixString = [Part]

toNixString :: Text -> NixString
toNixString = map quoteNonAlpha . Text.unpack where
  quoteNonAlpha :: Char -> Part
  quoteNonAlpha char | isAlpha char = Literal char
                     | otherwise = Escaped char

fromNixString :: StringKind -> NixString -> Text
fromNixString kind = Text.concat . map x where
  quote :: Text
  quote = case kind of
    SingleQuotes -> "''\\"
    DoubleQuote -> "\\"
  x :: Part -> Text
  x (Literal char) = Text.singleton char
  x (Escaped char) = quote <> Text.singleton char

minimizeStep :: StringKind -> NixString -> Maybe NixString
minimizeStep _ [] = Nothing
minimizeStep SingleQuotes (Escaped '\'' : Escaped '\'' : rest) = Just $ Literal '\'' : Literal '\'' : Literal '\'' : rest
minimizeStep SingleQuotes (Escaped '$' : Escaped '{' : rest) = Just $ Literal '\'' : Literal '\'' : Literal '$' : Literal '{' : rest
minimizeStep DoubleQuote (Escaped '$' : Escaped '{' : rest) = Just $ Escaped '$' : Literal '{' : rest
minimizeStep _ (Escaped c : rest) = Just $ Literal c : rest
minimizeStep _ (Literal _ : _) = Nothing

minimizeOnce :: StringKind -> NixString -> NixString -> Text -> IO NixString
minimizeOnce kind prefix str target =
  case minimizeStep kind str of
    Nothing -> tailMinimize str target
    Just newStr -> do
      stillValid <- check kind (prefix ++ newStr) target
      if stillValid then
        minimizeOnce kind prefix newStr target
      else
        tailMinimize str target
  where
    tailMinimize :: NixString -> Text -> IO NixString
    tailMinimize [] _ = return []
    tailMinimize (c:cs) target' = do
      minimizedRest <- minimizeOnce kind (prefix ++ [c]) cs target'
      return $ c : minimizedRest

minimizeFull :: StringKind -> NixString -> Text -> IO NixString
minimizeFull kind str target = do
  newStr <- minimizeOnce kind [] str target
  if str /= newStr then
    minimizeFull kind newStr target
  else return str

data StringKind = SingleQuotes | DoubleQuote

check :: StringKind -> NixString -> Text -> IO Bool
check kind nix target' = do
  let target = target' <> " "
      (pre, post) = prepost
      nixString = pre <> fromNixString kind nix <> post
      evalOptions = (defNixEvalOptions (Left (LBS.fromStrict (encodeUtf8 nixString))))
        { mode = Json
        }
  result <- nixInstantiate "nix-instantiate" evalOptions
  case result of
    Left _ -> return False
    Right jsonString -> case decode' jsonString of
      Nothing -> return False
      Just evalResult -> return (target == evalResult)
  where
    prepost :: (Text, Text)
    prepost = case kind of
      SingleQuotes -> ("''", " ''")
      DoubleQuote -> ("\"", " \"")



escapeHandle :: Text -> PluginT App ()
escapeHandle text' = do
  let text = Text.dropWhile isSpace text'
  if Text.null text then
    reply "Usage: ,escape <text> to show how to escape the given text in Nix"
  else do
    single <- liftIO $ fromNixString SingleQuotes <$> minimizeFull SingleQuotes (toNixString text) text
    double <- liftIO $ fromNixString DoubleQuote <$> minimizeFull DoubleQuote (toNixString text) text

    if single == double then
      reply $ "Escape this in \" and '' strings with: " <> single
    else do
      reply $ "Escape this in '' strings with: " <> single
      reply $ "Escape this in \" strings with: " <> double
