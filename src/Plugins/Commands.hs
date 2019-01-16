{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.Commands (commandsPlugin') where

import           Control.Monad.IO.Class
import           Data.Functor               (($>))
import           IRC                        hiding (paging)
import           Plugins
import           Plugins.Commands.Dynamic
import           Plugins.Commands.Find
import           Plugins.Commands.Locate
import           Plugins.Commands.Shared
import           Plugins.Commands.Tell
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

data Command = Find Find
             | Tell Tell
             | Locate Locate
             | Dynamic Dynamic
             | Listing (Maybe Int)
             deriving (Show)

parseCommand :: Parser Command
parseCommand = Listing <$> listingParser
  <|> word "find" *> (Find <$> findParser)
  <|> word "tell" *> (Tell <$> tellParser)
  <|> word "locate" *> (Locate <$> locateParser)
  <|> Dynamic <$> dynamicParser

handleCommand :: (MonadIO m, PluginMonad m, IRCMonad m) => Command -> m ()
handleCommand (Tell tell)       = tellHandle tell
handleCommand (Find find')      = findHandle find'
handleCommand (Locate locate)   = locateHandle locate
handleCommand (Dynamic dynamic) = dynamicHandle dynamic
handleCommand (Listing _)       = reply "Listing currently unimplemented"

listingParser :: Parser (Maybe Int)
listingParser = eof $> Nothing
  <|> Just <$> lexeme L.decimal

commandsPlugin' :: Plugin
commandsPlugin' = Plugin
  { pluginName = "commands"
  , pluginCatcher = \Input { inputMessage } -> case inputMessage of
      ',':command -> Catched True $ parse parseCommand "(message)" command
      _           -> PassedOn
  , pluginHandler = \case
      Left err -> do
        liftIO $ print err
        reply "Invalid command syntax"
      Right command -> handleCommand command
  }
