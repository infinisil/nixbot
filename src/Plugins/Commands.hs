{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Plugins.Commands (commandsPlugin) where

import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.ByteString.Char8   as BS
import           Data.Char
import           Data.Either
import           Data.Either.Combinators
import           Data.List               (findIndex, intercalate, isSuffixOf,
                                          minimumBy, sortBy, sortOn)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, fromJust, fromMaybe,
                                          listToMaybe, mapMaybe, maybeToList)
import           Data.Ord                (comparing)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import           Data.Versions
import           Plugins
import           System.Exit
import           System.Process

import           Data.Aeson
import           Text.EditDistance       (defaultEditCosts, levenshteinDistance)
import           Text.Read               (readMaybe)

import           Control.Monad.State
import           NixEval
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

nixLocate' :: MonadIO m => LocateMode -> Bool -> String -> m (Either String [String])
nixLocate' mode whole file = do
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "/run/current-system/sw/bin/nix-locate"
    ("--minimal":"--top-level":argsForMode mode file ++ [ "--whole-name" | whole ]) ""
  return $ case exitCode of
    ExitFailure code -> Left $ "nix-locate: Error(" ++ show code ++ "): " ++ show stderr ++ show stdout
    ExitSuccess -> Right $ lines stdout

nixLocate :: MonadIO m => LocateMode -> String -> m (Either String [String])
nixLocate mode file = do
  whole <- nixLocate' mode True file
  all <- case whole of
    Left error -> return $ Left error
    Right []   -> nixLocate' mode False file
    Right _    -> return whole
  mapM refinePackageList all

byBestNames :: [(String, String)] -> [String]
byBestNames names = bestsStripped
  where
    pnames :: [(String, (String, String))]
    pnames = map (\(attr, name) -> (attr, parseDrvName . init . tail $ name)) names
    -- Map from package name to (Attribute, version)
    nameMap :: Map String [(String, String)]
    nameMap = foldr (\(attr, (pname, version)) -> M.insertWith (++) pname [(attr, version)]) M.empty pnames
    chooseBestVersion :: [(String, String)] -> String
    chooseBestVersion pairs = fst . last $ y
      where
        x = map (\(l, r) -> (l, rightToMaybe . versioning . Text.pack $ r)) pairs
        y = sortBy (\(lattr, lvers) (rattr, rvers) ->
                      compare (length rattr) (length lattr)
                   ) x
    bests = map chooseBestVersion $ M.elems nameMap
    bestsStripped = map (stripSuffix ".out") bests

splitAttrPath :: String -> [String]
splitAttrPath path = map Text.unpack $ Text.split (=='.') (Text.pack path)

refinePackageList :: MonadIO m => [String] -> m [String]
refinePackageList packages = do
  liftIO $ print $ "Called with " ++ show packages
  let nixattrs = "[" ++ concatMap (\p -> "[" ++ concatMap (\a -> "\"" ++ a ++ "\"") (splitAttrPath p ++ ["name"]) ++ "]") packages ++ "]"
  result <- nixInstantiate def
    { contents = "with import <nixpkgs> { config = { allowUnfree = true; allowBroken = true; allowUnsupportedSystem = true; }; }; map (path: lib.attrByPath path null pkgs) " ++ nixattrs
    , mode = Json
    , attributes = []
    , nixPath = [ "nixpkgs=/var/lib/nixbot/state/nixpkgs" ]
    , transform = id
    }
  case result of
    Left error      -> return []
    Right namesJson -> do
      liftIO $ print $ "Got nixi result: " ++ namesJson
      case decodeStrict (BS.pack namesJson) :: Maybe [Maybe String] of
        Nothing -> do
          liftIO $ putStrLn "Can't decode input"
          return []
        Just names -> return $ sortBy (\a b -> compare (length a) (length b)) . byBestNames . mapMaybe sequence . zip packages $ names

stripSuffix :: String -> String -> String
stripSuffix suffix string = if isSuffixOf suffix string then
  take (length string - length suffix) string else string

parseDrvName :: String -> (String, String)
parseDrvName name = case splitIndex of
  Nothing -> (name, "")
  Just index -> let (left, right) = splitAt index name in
    (left, tail right)
  where
    splitIndex = findIndex (uncurry (&&) . bimap (=='-') (not . isAlpha)) . zip name . tail $ name

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
