{-# LANGUAGE FlexibleContexts #-}
module Nix.Session.Input where

import           Data.Char
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

import           Control.Monad.Combinators
import           Control.Monad.Except
import           System.Directory
import           System.Exit
import           System.Process               hiding (Inherit)

import           Nix.Expr
import           Nix.Parser
import           Text.Megaparsec              (eof, try)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

data InputError = HNixParseFailure Doc
                | NixParseFailure String deriving Show

data Input = Nix NixAction
           | Command ()
           | Nop
           deriving Show

data NixAction = Assignments [Binding NExprLoc]
               | Evaluation NExprLoc
               deriving Show

parseInput :: (MonadIO m, MonadError InputError m) => Text -> m Input
parseInput input = case Text.uncons input of
  Just (':', rest) -> return $ Command $ parseCommand input
  _ -> if Text.all isSpace input
         then return Nop
         else Nix <$> parseNix input

parseCommand :: Text -> ()
parseCommand str = ()

isAssignmentParser = whiteSpace *> ((nixSelector *> symbol "=") <|> symbol "inherit")

parseNix :: (MonadIO m, MonadError InputError m) => Text -> m NixAction
parseNix input = if isAssignmentParser `canParse` input
  then Assignments <$> parseNixAssign input
  else Evaluation <$> parseNixEval input
  where

parseNixEval :: (MonadIO m, MonadError InputError m) => Text -> m NExprLoc
parseNixEval = comparingParse (whiteSpace *> nixToplevelForm <* eof) ("with null; " <>)

parseNixAssign :: (MonadIO m, MonadError InputError m) => Text -> m [Binding NExprLoc]
parseNixAssign = comparingParse (whiteSpace *> nixBinders <* eof) (\t -> "with null; { " <> t <> " }")

standardNixParse :: (MonadIO m, MonadError String m) => String -> m ()
standardNixParse input = do
  -- TODO: Make a read env to save this
  --liftIO $ putStrLn $ "Doing nix-instantiate with: " ++ input
  nixInstantiateExe <- liftIO $ fromJust <$> findExecutable "nix-instantiate"
  let args = ["--parse", "-E", input ]
  (exitCode, _, stderr) <- liftIO $ readProcessWithExitCode nixInstantiateExe args ""
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure _ -> throwError stderr

comparingParse :: (MonadIO m, MonadError InputError m) => Parser a -> (Text -> Text) -> Text -> m a
comparingParse parser toplevelMod input = do
  nixResult <- runExceptT $ standardNixParse toplevel
  case (hnixResult, nixResult) of
    (Success val, Right _) -> return val
    (Failure _, Left err) -> throwError $ NixParseFailure err
    (Success _, Left err) -> do
      liftIO $ putStrLn $ "hnix succeeds in parsing this invalid Nix expression: " ++ toplevel
      throwError $ NixParseFailure err
    (Failure doc, Right _) -> do
      liftIO $ putStrLn $ "hnix fails to parse this valid Nix expression: " ++ toplevel
      throwError $ HNixParseFailure doc
  where
    hnixParse = parseFromText parser

    (selectedInput, hnixResult) = case hnixParse inputWithSemi of
      Failure _   -> (input, hnixParse input)
      Success val -> (inputWithSemi, Success val)
      where inputWithSemi = input <> ";"

    toplevel = Text.unpack $ toplevelMod selectedInput


canParse :: Parser a -> Text -> Bool
canParse parser input = case parser `parseFromText` input of
  Success _ -> True
  Failure _ -> False


