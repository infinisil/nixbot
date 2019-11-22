{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.Commands (commandsPlugin') where

import           Control.Monad.IO.Class
import           Data.Functor               (($>))
import qualified Data.Text                  as Text
import           Frontend.Types
import           Plugins
import           Plugins.Commands.Dynamic
import           Plugins.Commands.Expand
import           Plugins.Commands.Find
import           Plugins.Commands.Guys
import           Plugins.Commands.Locate
import           Plugins.Commands.Shared
import           Plugins.Commands.Tell
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import           Types

data Command = Find Find
             | Tell Tell
             | Locate Locate
             | Dynamic Dynamic
             | Listing (Maybe Int)
             | Expand ExpandCommand
             | Guys GuysCommand
             deriving (Show)

parseCommand :: Parser Command
parseCommand = Listing <$> listingParser
  <|> word "find" *> (Find <$> findParser)
  <|> word "tell" *> (Tell <$> tellParser)
  <|> word "locate" *> (Locate <$> locateParser)
  <|> word "expand" *> (Expand <$> expandParser)
  <|> word "guys" *> (Guys <$> guysParser)
  <|> Dynamic <$> dynamicParser

handleCommand :: Command -> PluginT App ()
handleCommand (Tell tell)       = tellHandle tell
handleCommand (Find find')      = findHandle find'
handleCommand (Locate locate)   = locateHandle locate
handleCommand (Dynamic dynamic) = do
  chan <- getChannel
  case (chan, dynamic) of
    (Just _, _)                 -> dynamicHandle dynamic
    (Nothing, DynamicQuery _ _) -> dynamicHandle dynamic
    _                           -> reply "Not allowed in PMs"
handleCommand (Expand expand) = expandHandle expand
handleCommand (Guys expand) = guysHandle expand
handleCommand (Listing listing)       = do
  let special = ["find", "tell", "locate", "expand", "guys"]
  answer <- listCommands special listing
  reply answer

listingParser :: Parser (Maybe Int)
listingParser = eof $> Nothing
  <|> Just <$> lexeme L.decimal

commandsPlugin' :: Plugin
commandsPlugin' = Plugin
  { pluginName = "commands"
  , pluginCatcher = \Input { inputMessage } -> case Text.uncons inputMessage of
      Just (',', command) -> Catched True $ parse parseCommand "(message)" (Text.unpack command)
      _           -> PassedOn
  , pluginHandler = \case
      Left err -> do
        liftIO $ print err
        reply "Invalid command syntax"
      Right command -> handleCommand command
  }
