{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Plugins.NixRepl (nixreplPlugin) where

import           Control.Applicative        ((<|>))
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.State.Class
import           Data.List
import           Data.Maybe                 (fromMaybe, listToMaybe,
                                             maybeToList)
import           Plugins
import           System.Exit                (ExitCode (..))
import           System.Process             (readProcessWithExitCode)
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Map                   (Map)
import qualified Data.Map                   as M

data Instruction = Definition String String
                 | Evaluation String
                 | Command String [String]
                 deriving Show

data NixState = NixState
  { variables :: Map String String
  , scopes :: [ String ]
  } deriving (Show, Read)

type Parser = P.Parsec () String

parser :: Parser Instruction
parser =
  P.try cmdParser <|> P.try defParser <|> Evaluation <$> (C.space *> P.takeRest)
    where
      literal :: Parser String
      literal = (:) <$> (C.letterChar <|> C.char '_') <*> P.many C.alphaNumChar

      cmdParser :: Parser Instruction
      cmdParser = do
        C.space
        C.char ':'
        cmd <- literal
        args <- P.many (C.space *> P.some (C.notChar ' '))
        return $ Command cmd args

      defParser :: Parser Instruction
      defParser = do
        C.space
        lit <- literal
        C.space
        C.char '='
        C.space
        value <- P.takeRest
        return $ Definition lit value

nixpkgs = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz"
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
                            ] ] ++ [ "--show-trace", "-" ]

outputTransform :: String -> String
outputTransform = take 200 . fromMaybe "(no output)" . listToMaybe . take 1 . reverse . lines

nixInstantiate :: MonadIO m => Bool -> String -> m (Either String String)
nixInstantiate eval contents = do
  let options = (if eval then "--eval" else "--parse") : nixInstantiateOptions
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode nixInstantiatePath options contents
  case exitCode of
    ExitSuccess      -> return . Right $ outputTransform stdout
    ExitFailure code -> return . Left $ outputTransform stderr


nixFile :: NixState -> String -> String
nixFile (NixState { variables, scopes }) lit = "let\n"
    ++ concatMap (\(lit, val) -> "\t" ++ lit ++ " = " ++ val ++ ";\n") (M.assocs (M.union variables defaultVariables))
    ++ "in \n"
    ++ concatMap (\scope -> "\twith " ++ scope ++ ";\n") (reverse scopes)
    ++ "\t" ++ lit

tryMod :: (MonadIO m, MonadState NixState m) => (NixState -> NixState) -> m (Maybe String)
tryMod mod = do
  newState <- gets mod
  let contents = nixFile newState "null"
  liftIO . putStrLn $ "Trying to modify nix state to:\n" ++ contents ++ "\n"
  result <- nixInstantiate False contents
  case result of
    Right _ -> do
      put newState
      return Nothing
    Left error -> return $ Just error

handle :: (MonadIO m, MonadState NixState m) => Instruction -> m (Maybe String)
handle (Definition lit val) = tryMod (\s -> s { variables = M.insert lit val (variables s) })
handle (Evaluation lit) = do
  state <- get
  let contents = nixFile state ("_show (" ++ lit ++ ")")
  liftIO . putStrLn $ "Trying to evaluate " ++ lit ++ " in nix file: \n" ++ contents ++ "\n"
  result <- nixInstantiate True contents
  case result of
    Right value -> return $ Just value
    Left error  -> return $ Just error
handle (Command "l" []) = return $ Just ":l needs an argument"
handle (Command "l" args) = tryMod (\s -> s { scopes = unwords args : scopes s } )
handle (Command cmd _) = return . Just $ "Unknown command: " ++ cmd

defaultVariables :: Map String String
defaultVariables = M.fromList
  [ ("_show", "x: x")
  , ("nixpkgs", "builtins.fetchTarball \"" ++ nixpkgs ++ "\"")
  , ("pkgs", "import nixpkgs {}")
  , ("lib", "pkgs.lib")
  ]

nixreplPlugin :: (MonadIO m, MonadLogger m, Monad m) => MyPlugin NixState m
nixreplPlugin = MyPlugin initialState trans "nixrepl"
  where
    initialState = NixState M.empty []
    trans (_, '>':nixString) = case P.runParser parser "(input)" nixString of
      Right instruction -> maybeToList <$> handle instruction
      Left _            -> return []
    trans _ = return []
