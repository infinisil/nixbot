{-# LANGUAGE FlexibleContexts      #-}
module Plugins.NixRepl (nixreplPlugin) where

import Plugins
import Data.Maybe (maybeToList, listToMaybe, fromMaybe)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.State.Class
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List

import qualified Data.Map as M
import Data.Map (Map)

data Def = Def String String
         deriving Show

data Instruction = Definition Def
                 | Evaluation String
                 deriving Show

type NixState = Map String String

type Parser = P.Parsec () String

parser :: Parser Instruction
parser = 
  P.try (Definition <$> defParser) <|> Evaluation <$> (C.space *> P.takeRest)
    where
      literal :: Parser String
      literal = (:) <$> (C.letterChar <|> C.char '_') <*> P.many C.alphaNumChar

      defParser :: Parser Def
      defParser = do
        C.space
        lit <- literal
        C.space
        C.string "="
        C.space
        value <- P.takeRest
        return $ Def lit value

nixInstantiatePath = "/run/current-system/sw/bin/nix-instantiate"
nixInstantiateOptions = concat [ ["--option", var, val] | (var, val) <-
                            [ ("cores", "0")
                            , ("fsync-metadata", "false")
                            , ("restrict-eval", "true")
                            , ("sandbox", "true")
                            , ("timeout", "3")
                            , ("max-jobs", "1")
                            , ("allow-import-from-derivation", "false")
                            , ("allowed-uris", nixpkgs)
                            ] ] ++ [ "-I", "nixpkgs=" ++ nixpkgs, "--show-trace", "-" ]
                        where
                          nixpkgs = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz"

outputTransform :: String -> String
outputTransform = take 200 . fromMaybe "(no output)" . listToMaybe . take 1 . reverse . lines

nixInstantiate :: MonadIO m => Bool -> String -> m (Either String String)
nixInstantiate eval contents = do
  let options = (if eval then "--eval" else "--parse") : nixInstantiateOptions
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode nixInstantiatePath options contents
  case exitCode of
    ExitSuccess -> return . Right $ outputTransform stdout
    ExitFailure code -> return . Left $ outputTransform stderr
  

nixFile :: NixState -> String -> String
nixFile state lit = "let\n"
    ++ concatMap (\(lit, val) -> "\t" ++ lit ++ " = " ++ val ++ ";\n") (M.assocs state)
    ++ "in " ++ lit

handle :: (MonadIO m, MonadState NixState m) => Instruction -> m (Maybe String)
handle (Definition (Def lit val)) = do
  potNewState <- gets $ M.insert lit val
  let contents = nixFile potNewState "null"
  liftIO . putStrLn $ "Trying to validate new definition for " ++ lit ++ " in nix file: \n" ++ contents ++ "\n"
  result <- nixInstantiate False contents
  case result of
    Right _ -> do
      put potNewState
      return . Just $ lit ++ " defined"
    Left error -> return $ Just error
handle (Evaluation lit) = do
  state <- get
  let contents = nixFile state ("_show (" ++ lit ++ ")")
  liftIO . putStrLn $ "Trying to evaluate " ++ lit ++ " in nix file: \n" ++ contents ++ "\n"
  result <- nixInstantiate True contents
  case result of
    Right value -> return $ Just value
    Left error -> return $ Just error

defaults :: NixState
defaults = M.fromList
  [ ("_show", "x: x")
  , ("pkgs", "import <nixpkgs> {}")
  , ("lib", "pkgs.lib")
  ]

nixreplPlugin :: (MonadIO m, MonadLogger m, Monad m) => MyPlugin (Map String String) m
nixreplPlugin = MyPlugin defaults trans "nixrepl"
  where
    trans (_, '>':nixString) = case P.runParser parser "(input)" nixString of
      Right instruction -> maybeToList <$> handle instruction
      Left _ -> return []
    trans _ = return []
