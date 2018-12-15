{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Plugins.Commands (commandsPlugin) where

import           Control.Applicative        (liftA2)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Either
import           Data.Either.Combinators
import           Data.List                  (findIndex, intercalate, isSuffixOf,
                                             minimumBy, sortBy, sortOn)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe,
                                             listToMaybe, mapMaybe, maybeToList)
import           Data.Ord                   (comparing)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Data.Versions
import           Data.Void
import           NixEval
import           Plugins
import           System.Directory           (findExecutable)
import           System.Exit
import           System.Process
import           Text.EditDistance          (defaultEditCosts,
                                             levenshteinDistance)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)
import           Text.Read                  (readMaybe)
import           Utils

data LookupResult = Empty | Exact String | Guess String String

replyLookup :: String -> Maybe String -> LookupResult -> [String]
replyLookup _ _ Empty = []
replyLookup _ Nothing (Exact str) = [str]
replyLookup _ (Just arg) (Exact str) = [arg ++ ": " ++ str]
replyLookup nick Nothing (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", str]
replyLookup nick (Just arg) (Guess key str) = [nick ++ ": Did you mean " ++ key ++ "?", arg ++ ": " ++ str]

lookupCommand :: MonadState (M.Map String (Int, String)) m => String -> m LookupResult
lookupCommand str = do
  assos <- map (\(k, (uses, v)) -> (levenshteinDistance defaultEditCosts str k, (k, v))) <$> gets M.assocs
  let result = if null assos then Nothing else Just $ minimumBy (comparing fst) assos
  case result of
    Nothing -> return Empty
    Just (0, (word, value)) -> do
      modify $ M.adjust (\(uses, v) -> (uses + 1, v)) word
      return $ Exact value
    Just (dist, (word, value)) -> do
      let isValid = fromIntegral dist / fromIntegral (length word) < 0.34
      return $ if isValid then Guess word value else Empty

data LocateMode = Generic
                | Bin
                | Man

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



data NixLocateResult = NixLocateResult
  { attrPath :: [String]
  , size     :: Int
  , fileType :: Char
  , path     :: FilePath
  } deriving Show

type Parser = Parsec Void String

nixLocateParser :: Parser [NixLocateResult]
nixLocateParser = many line <* eof where
  line :: Parser NixLocateResult
  line = NixLocateResult
    <$> (many (noneOf ". ") `sepBy` char '.' <?> "attribute path") <* space
    <*> (toNum <$> decimal `sepBy` char ',' <?> "file size") <* char ' '
    <*> (anySingleBut ' ' <?> "file type") <* char ' '
    <*> (many (anySingleBut '\n') <?> "file path") <* newline

  toNum :: [Int] -> Int
  toNum = sum . zipWith (*) (iterate (*1000) 1) . reverse

nixLocate' :: MonadIO m => LocateMode -> Bool -> String -> m (Either String [NixLocateResult])
nixLocate' mode whole file = do
  locateBin <- liftIO $ fromMaybe (error "Couldn't find nix-locate executable") <$> findExecutable "nix-locate"
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode locateBin
    ("--top-level":argsForMode mode file ++ [ "--whole-name" | whole ]) ""
  case exitCode of
    ExitFailure code -> return $ Left $ "nix-locate: Error(" ++ show code ++ "): " ++ show stderr ++ show stdout
    ExitSuccess -> case parse nixLocateParser "nix-locate-output" stdout of
      Left err  -> do
        liftIO $ putStrLn stdout
        liftIO $ parseTest nixLocateParser stdout
        return $ Left $ "nix-locate output parsing error: " ++ show err
      Right res -> return $ Right res

nixLocate :: MonadIO m => LocateMode -> String -> m (Either String [String])
nixLocate mode file = do
  whole <- nixLocate' mode True file
  fmap selectAttrs <$> case whole of
    Left error -> return $ Left error
    Right []   -> nixLocate' mode False file
    Right _    -> return whole

selectAttrs :: [NixLocateResult] -> [String]
selectAttrs input = sortOn (liftA2 (,) length id) $ map (stripSuffix ".out" . intercalate "." . attrPath) input

stripSuffix :: String -> String -> String
stripSuffix suffix string = if isSuffixOf suffix string then
  take (length string - length suffix) string else string

ircLimit :: String -> Bool
ircLimit = (<456) . length

doNixLocate :: MonadIO m => LocateMode -> String -> m String
doNixLocate mode arg = do
  attributes <- nixLocate mode arg
  return $ case attributes of
    Left error -> error
    Right [] -> "Couldn't find in any packages"
    Right packages -> case mostMatching packages present ircLimit of
      Nothing -> "Found in packages, but the package attribute is too long for an IRC message.."
      Just result -> result
      where
        present (shown, hidden) = "Found in packages: " ++ intercalate ", " shown ++
          if null hidden then "" else ", and " ++ show (length hidden) ++ " more"

fixed :: Map String (String -> [String] -> StateT St IO (Maybe String))
fixed = M.fromList
  [ ("tell", const $ \args -> return Nothing)
  , ("find", const $ \args -> return Nothing)
  , ("locate", const $ \args -> case args of
    [] -> return $ Just "Use ,locate <filename> to find packages containing such a file. Powered by nix-index (local installation recommended)."
    [arg] -> Just <$> doNixLocate Generic arg
    "bin":[arg] -> Just <$> doNixLocate Bin arg
    "man":[arg] -> Just <$> doNixLocate Man arg
    [tp, _] -> return $ Just $ "Unknown locate type " ++ tp
    _ -> return $ Just ",locate only takes 1 or 2 arguments"
    )
  ]

listCommands :: MonadState St m => Maybe Int -> m String
listCommands mpage = do
  sorted <- gets $ fmap fst . sortBy (flip $ comparing snd) . M.assocs . M.map fst
  let pages = paging sorted getPage ircLimit
  return $ if 0 <= page && page < length pages then pages !! page else "Invalid page index, the last page is number " ++ show (length pages - 1)

  where
    special = M.keys fixed
    getPage 0 items = "Special commands: " ++ unwords special
      ++ " - Commands sorted by use count, page 0 (use ,<n> to view page <n>): " ++ unwords items
    getPage n items = "Page " ++ show n ++ ": " ++ unwords items
    page = fromMaybe 0 mpage



type St = Map String (Int, String)

commandsPlugin :: MonadIO m => MyPlugin St m
commandsPlugin = MyPlugin M.empty trans "commands"
  where
    trans (chan, nick, ',':command) = case words command of
      [] -> (:[]) <$> listCommands Nothing
      cmd:rest -> case readMaybe cmd :: Maybe Int of
        Just num -> (:[]) <$> listCommands (Just num)
        Nothing -> case M.lookup cmd fixed of
          Just fun -> do
            cur <- get
            (a, s) <- liftIO $ runStateT (fun nick rest) cur
            put s
            return $ maybeToList a
          Nothing -> case rest of
            [] -> replyLookup nick Nothing <$> lookupCommand cmd
            "=":vals -> case length vals of
                0 -> do
                  mvalue <- gets $ M.lookup cmd
                  case mvalue of
                    Nothing -> return [ cmd ++ " is already undefined" ]
                    Just (_, value) -> do
                      modify (M.delete cmd)
                      return [ "Undefined " ++ cmd ++ ", was defined as: " ++ value]
                _ -> do
                  old <- gets $ M.lookup cmd
                  modify $ M.insertWith (\(_, new) (uses, _) -> (uses, new)) cmd (0, unwords vals)
                  case old of
                    Nothing -> return [ cmd ++ " defined" ]
                    Just (_, val) -> return [ cmd ++ " redefined, was defined as: " ++ val ]
            args -> replyLookup nick (Just (unwords args)) <$> lookupCommand cmd

    trans (_, _, _) = return []
