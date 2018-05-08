{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.NixRepl (nixreplPlugin) where

import           Config
import           NixEval
import           Plugins

import           Control.Applicative        ((<|>))
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.State.Class
import           Data.List
import           Data.Maybe                 (fromMaybe, listToMaybe,
                                             maybeToList)
import           System.Directory
import           System.Exit                (ExitCode (..))
import           System.Process
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
  , scopes    :: [ String ]
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
        C.space
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

nixFile :: NixState -> String -> String
nixFile NixState { variables, scopes } lit = "let\n"
    ++ concatMap (\(lit, val) -> "\t" ++ lit ++ " = " ++ val ++ ";\n") (M.assocs (M.union variables defaultVariables))
    ++ "in \n"
    ++ concatMap (\scope -> "\twith " ++ scope ++ ";\n") (reverse scopes)
    ++ "\t" ++ lit

nixpkgs :: MonadReader Config m => m String
nixpkgs = (++ "/nixpkgs") <$> reader stateDir

nixEval :: (MonadReader Config m, MonadIO m) => String -> Bool -> m (Either String String)
nixEval contents eval = do
  nixpkgs <- nixpkgs
  nixInstantiate def
    { contents = contents
    , mode = if eval then Lazy else Parse
    , nixPath = [ "nixpkgs=" ++ nixpkgs ]
    , options = publicOptions
    }

tryMod :: (MonadReader Config m, MonadIO m, MonadState NixState m) => (NixState -> NixState) -> m (Maybe String)
tryMod mod = do
  newState <- gets mod
  let contents = nixFile newState "null"
  liftIO . putStrLn $ "Trying to modify nix state to:\n" ++ contents ++ "\n"
  result <- nixEval contents False
  case result of
    Right _ -> do
      put newState
      return Nothing
    Left error -> return $ Just error

handle :: (MonadReader Config m, MonadIO m, MonadState NixState m) => Instruction -> m (Maybe String)
handle (Definition lit val) = do
  result <- tryMod (\s -> s { variables = M.insert lit val (variables s) })
  case result of
    Nothing    -> return . Just $ lit ++ " defined"
    Just error -> return $ Just error
handle (Evaluation lit) = do
  state <- get
  let contents = nixFile state ("_show (" ++ lit ++ ")")
  liftIO . putStrLn $ "Trying to evaluate " ++ lit ++ " in nix file: \n" ++ contents ++ "\n"
  result <- nixEval contents True
  case result of
    Right value -> return $ Just value
    Left error  -> return $ Just error
handle (Command "l" []) = return $ Just ":l needs an argument"
handle (Command "l" args) = do
  result <- tryMod (\s -> s { scopes = unwords args : scopes s } )
  case result of
    Nothing    -> return $ Just "imported scope"
    Just error -> return $ Just error
handle (Command "v" [var]) = do
  val <- gets $ M.findWithDefault (var ++ " is not defined") var . flip M.union defaultVariables . variables
  return . Just $ var ++ " = " ++ val
handle (Command "v" _) = do
  vars <- gets $ M.keys . flip M.union defaultVariables . variables
  return . Just $ "All bindings: " ++ unwords vars
handle (Command "s" _) = do
  scopes <- gets scopes
  return . Just $ "All scopes: " ++ intercalate ", " scopes
handle (Command "d" [lit]) = do
  litDefined <- gets $ M.member lit . variables
  if litDefined
    then do
      modify (\s -> s { variables = M.delete lit (variables s) })
      return . Just $ "undefined " ++ lit
    else return . Just $ lit ++ " is not defined"
handle (Command "d" _) = return $ Just ":d takes a single argument"
handle (Command "r" ["s"]) = do
  modify (\s -> s { scopes = [] })
  return $ Just "Scopes got reset"
handle (Command "r" ["v"]) = do
  modify (\s -> s { variables = M.empty })
  return $ Just "Variables got reset"
handle (Command "r" _) = do
  put $ NixState M.empty []
  return $ Just "State got reset"
handle (Command "u" _) = do
  updateNixpkgs
  return $ Just "Updated nixpkgs"
handle (Command cmd _) = return . Just $ "Unknown command: " ++ cmd

defaultVariables :: Map String String
defaultVariables = M.fromList
  [ ("_show", "x: x")
  , ("pkgs", "import <nixpkgs> {}")
  , ("lib", "pkgs.lib")
  ]

git :: MonadIO m => [String] -> m String
git args = liftIO $ do
  putStrLn $ "Calling git with arguments " ++ unwords args
  readProcess "/run/current-system/sw/bin/git" args ""

updateNixpkgs :: (MonadReader Config m, MonadIO m) => m String
updateNixpkgs = do
  nixpkgs <- nixpkgs
  exists <- liftIO $ doesPathExist nixpkgs
  result <- git $ if exists
    then ["-C", nixpkgs, "pull"]
    else ["clone", "https://github.com/NixOS/nixpkgs.git", nixpkgs]
  liftIO $ putStrLn result
  git ["-C", nixpkgs, "rev-parse", "HEAD"]

nixreplPlugin :: (MonadReader Config m, MonadIO m, MonadLogger m, Monad m) => MyPlugin NixState m
nixreplPlugin = MyPlugin initialState trans "nixrepl"
  where
    initialState = NixState M.empty []
    trans (_, '>':nixString) = case P.runParser parser "(input)" nixString of
      Right instruction -> maybeToList <$> handle instruction
      Left _            -> return []
    trans _ = return []
