{-# LANGUAGE OverloadedStrings #-}

module Plugins.Commands.Locate
  ( Locate
  , locateParser
  , locateHandle
  ) where

import           Control.Arrow              ((&&&))
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Void
import           IRC
import           Plugins
import           System.Directory
import           System.Exit
import           System.Process
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Types
import           Utils

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme n = try $ n <* (void space1 <|> eof)

word :: String -> Parser ()
word n = lexeme (void $ string n)

parseWord :: Parser String
parseWord = some (satisfy (not . isSpace)) <* (void space1 <|> eof)

data Locate = LocateHelp
            | LocateWithMode LocateMode String
            deriving (Show)

data LocateMode = Generic
                | Bin
                | Man
                deriving (Show)

locateParser :: Parser Locate
locateParser = eof $> LocateHelp
  <|> word "bin" *> (LocateWithMode Bin <$> parseWord)
  <|> word "man" *> (LocateWithMode Man <$> parseWord)
  <|> LocateWithMode Generic <$> parseWord

locateHandle :: Locate -> PluginT App ()
locateHandle LocateHelp = reply "Use ,locate <filename> to find packages containing such a file. Powered by nix-index (local installation recommended) https://github.com/bennofs/nix-index"
locateHandle (LocateWithMode mode str) = do
  result <- doNixLocate mode str
  reply result

stripSuffix :: String -> String -> String
stripSuffix suffix str = if suffix `isSuffixOf` str then
  take (length str - length suffix) str else str

doNixLocate :: MonadIO m => LocateMode -> String -> m Text
doNixLocate locateMode arg = do
  attrs <- nixLocate locateMode arg
  return $ case attrs of
    Left err -> Text.pack err
    Right [] -> "Couldn't find in any packages"
    Right packages -> fromMaybe "Found in packages, but the package attribute is too long for an IRC message.."
      $ mostMatching packages present ircLimit
      where
        present :: ([Text], [Text]) -> Text
        present (shown, extra) = "Found in packages: " <> Text.intercalate ", " shown <>
          if null extra then "" else ", and " <> Text.pack (show (length extra)) <> " more"

argsForMode :: LocateMode -> String -> [String]
argsForMode Generic arg =
  [ case arg of
    '/':_ -> arg
    _     -> '/':arg
  ]
argsForMode Bin arg =
  [ "--at-root"
  , "/bin/" ++ arg
  ]
argsForMode Man arg =
  [ "--regex"
  , "--at-root"
  , "/share/man/man[0-9]/" ++ arg ++ ".[0-9].gz"
  ]

selectAttrs :: [NixLocateResult] -> [Text]
selectAttrs = map Text.pack . sortOn (length &&& id) . nub . map (stripSuffix ".out" . intercalate "." . attrPath)

nixLocateParser :: Parser [NixLocateResult]
nixLocateParser = many line <* eof where
  line :: Parser NixLocateResult
  line = NixLocateResult
    <$> (many (noneOf (". " :: String)) `sepBy` char '.' <?> "attribute path") <* space
    <*> (toNum <$> L.decimal `sepBy` char ',' <?> "file size") <* char ' '
    <*> (anySingleBut ' ' <?> "file type") <* char ' '
    <*> (many (anySingleBut '\n') <?> "file path") <* newline

  toNum :: [Int] -> Int
  toNum = sum . zipWith (*) (iterate (*1000) 1) . reverse


data NixLocateResult = NixLocateResult
  { attrPath :: [String]
  , size     :: Int
  , fileType :: Char
  , path     :: FilePath
  } deriving Show

nixLocate' :: MonadIO m => LocateMode -> Bool -> String -> m (Either String [NixLocateResult])
nixLocate' locateMode whole file = do
  locateBin <- liftIO $ fromMaybe (error "Couldn't find nix-locate executable") <$> findExecutable "nix-locate"
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode locateBin
    ("--top-level":argsForMode locateMode file ++ [ "--whole-name" | whole ]) ""
  case exitCode of
    ExitFailure code -> return $ Left $ "nix-locate: Error(" ++ show code ++ "): " ++ show stderr ++ show stdout
    ExitSuccess -> case parse nixLocateParser "nix-locate-output" stdout of
      Left err  -> do
        liftIO $ putStrLn stdout
        liftIO $ parseTest nixLocateParser stdout
        return $ Left $ "nix-locate output parsing error: " ++ show err
      Right res -> return $ Right res

nixLocate :: MonadIO m => LocateMode -> String -> m (Either String [Text])
nixLocate locateMode file = do
  whole <- nixLocate' locateMode True file
  fmap selectAttrs <$> case whole of
    Left err -> return $ Left err
    Right [] -> nixLocate' locateMode False file
    Right _  -> return whole
