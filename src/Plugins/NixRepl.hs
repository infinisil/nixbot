{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.NixRepl (nixreplPlugin) where

import           Config
import           Frontend.Types
import           Log
import           NixEval
import           Plugins
import           Types

import           Control.Applicative        ((<|>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.Bifunctor             (bimap)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List
import           Data.Text                  (pack)
import qualified Data.Text                  as Text
import           GHC.Generics
import           System.Directory
import           System.FilePath
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as C

import           Data.Map                   (Map)
import qualified Data.Map                   as M


data Instruction = Definition String String
                 | Evaluation Bool String
                 | Command String [String]
                 deriving Show

data NixState = NixState
  { variables :: Map String String
  , scopes    :: [ String ]
  } deriving (Show, Read, Generic)

instance FromJSON NixState
instance ToJSON NixState

type Parser = P.Parsec () String

type ReplApp = StateT NixState (PluginT App)

parser :: Parser Instruction
parser =
  P.try cmdParser <|> P.try defParser <|> Evaluation False <$> (C.space *> P.takeRest)
    where
      literal :: Parser String
      literal = (:) <$> (C.letterChar <|> C.char '_') <*> P.many (C.alphaNumChar <|> C.char '_' <|> C.char '-' <|> C.char '\'')

      cmdParser :: Parser Instruction
      cmdParser = do
        C.space
        _ <- C.char ':'
        cmd <- literal
        case cmd of
          "p" -> Evaluation True <$> (C.space *> P.takeRest)
          _ -> do
            args <- P.many (C.space *> P.some (P.anySingleBut ' '))
            C.space
            return $ Command cmd args

      defParser :: Parser Instruction
      defParser = do
        C.space
        lit <- literal
        C.space
        _ <- C.char '='
        C.space
        Definition lit <$> P.takeRest

nixFile :: NixState -> String -> String
nixFile NixState { variables, scopes } lit = "let\n"
    ++ concatMap (\(l, val) -> "\t" ++ l ++ " = " ++ val ++ ";\n") (M.assocs (M.union variables defaultVariables))
    ++ "in \n"
    ++ concatMap (\scope -> "\twith " ++ scope ++ ";\n") (reverse scopes)
    ++ "\t" ++ lit

nixEval :: String -> EvalMode -> PluginT App (Either String String)
nixEval contents mode = do
  sender <- getSender
  pluginConfig <- lift $ asks (pluginConfigForSender sender . config)
  let nixPath = configNixPath $ configNixrepl pluginConfig
  let nixInstPath = "/run/current-system/sw/bin/nix-instantiate"
  res <- lift . liftIO $ nixInstantiate nixInstPath (defNixEvalOptions (Left (BS.pack contents)))
    { mode = mode
    , nixPath = nixPath
    , options = unsetNixOptions
      { allowImportFromDerivation = Just False
      , restrictEval = Just True
      , sandbox = Just True
      , showTrace = Just True
      }
    }
  return $ bimap (outputTransform . BS.unpack) (outputTransform . BS.unpack) res

tryMod :: (NixState -> NixState) -> ReplApp (Maybe String)
tryMod modi = do
  newState <- gets modi
  let contents = nixFile newState "null"
  result <- lift $ nixEval contents Parse
  case result of
    Right _ -> do
      put newState
      return Nothing
    Left err -> return $ Just err

handle :: Instruction -> ReplApp String
handle (Definition lit val) = do
  result <- tryMod (\s -> s { variables = M.insert lit val (variables s) })
  case result of
    Nothing  -> return $ lit ++ " defined"
    Just err -> return err
handle (Evaluation strict lit) = do
  st <- get
  let contents = nixFile st ("_show (\n" ++ lit ++ "\n)")
  result <- lift $ nixEval contents (if strict then Strict else Lazy)
  case result of
    Right value -> return value
    Left err    -> return err
handle (Command "l" []) = return ":l needs an argument"
handle (Command "l" args) = do
  result <- tryMod (\s -> s { scopes = unwords args : scopes s } )
  case result of
    Nothing  -> return "imported scope"
    Just err -> return err
handle (Command "v" [var]) = do
  val <- gets $ M.findWithDefault (var ++ " is not defined") var . flip M.union defaultVariables . variables
  return $ var ++ " = " ++ val
handle (Command "v" _) = do
  vars <- gets $ M.keys . flip M.union defaultVariables . variables
  return $ "All bindings: " ++ unwords vars
handle (Command "s" _) = do
  scopes <- gets scopes
  return $ "All scopes: " ++ intercalate ", " scopes
--handle (Command "d" [lit]) = do
--  litDefined <- gets $ M.member lit . variables
--  if litDefined
--    then do
--      modify (\s -> s { variables = M.delete lit (variables s) })
--      return . Just $ "undefined " ++ lit
--    else return . Just $ lit ++ " is not defined"
--handle (Command "d" _) = return $ Just ":d takes a single argument"
handle (Command "r" []) = do
  modify (\s -> s { scopes = [] })
  return "Scopes got reset"
--handle (Command "r" ["v"]) = do
--  modify (\s -> s { variables = M.empty })
--  return $ Just "Variables got reset"
--handle (Command "r" _) = do
--  put $ NixState M.empty []
--  return $ Just "State got reset"
handle (Command cmd _) = return $ "Unknown command: " ++ cmd

defaultVariables :: Map String String
defaultVariables = M.fromList
  [ ("_show", "x: x")
  , ("pkgs", "import <nixpkgs> {}")
  , ("lib", "pkgs.lib")
  ]

nixreplPlugin :: Plugin
nixreplPlugin = Plugin
  { pluginName = "nixrepl"
  , pluginCatcher = \Input { inputSender, inputMessage } -> case Text.unpack inputMessage of
      '>':' ':nixString -> case P.runParser parser "(input)" nixString of
        Right instruction -> Catched True (inputSender, instruction)
        Left _            -> PassedOn
      _ -> PassedOn
  , pluginHandler = \(sender, instruction) -> do
      stateFile <- (</> "state") <$> case sender of
        Left user    -> getUserState user
        Right (_, _) -> getGlobalState
      exists <- liftIO $ doesFileExist stateFile
      initialState <- if exists then
        liftIO (decodeFileStrict stateFile) >>= \case
          Just result -> return result
          Nothing -> do
            lift $ logMsg $ "Failed to decode nix state at " <> pack stateFile
            return $ NixState M.empty []
      else
        return $ NixState M.empty []

      (result, newState) <- runStateT (handle instruction) initialState
      case sender of
        Left user       -> privMsg user (pack result)
        Right (chan, _) -> chanMsg chan (pack result)
      liftIO $ encodeFile stateFile newState
  }
