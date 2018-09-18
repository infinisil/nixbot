{-# LANGUAGE FlexibleContexts #-}
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
                                          listToMaybe, mapMaybe)
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

import           Control.Monad.State
import           NixEval

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

doNixLocate :: MonadIO m => LocateMode -> String -> m String
doNixLocate mode arg = do
  attributes <- nixLocate mode arg
  return $ case attributes of
    Left error -> error
    Right packages -> uncurry (++) . bimap
      (\ps -> if null ps
        then "Couldn't find any packages"
        else "Found in packages: " ++ intercalate ", " ps)
      (\rest -> if null rest
        then ""
        else ", and " ++ show (length rest) ++ " more")
      . splitAt 7 $ packages

type St = Map String (Int, String)

commandsPlugin :: MonadIO m => MyPlugin St m
commandsPlugin = MyPlugin M.empty trans "commands"
  where
    trans (chan, nick, ',':command) = case words command of
      [] -> do
        els <- gets $ M.assocs . M.map fst
        let sorted = (' ':) . fst <$> sortBy (flip $ comparing snd) els
        let res = scanl (++) "Most used commands: " sorted
        let x = last $ takeWhile ((<456).length) res
        return [ x ]

      "locate":args -> case args of
        [] -> return ["Use ,locate <filename> to find packages containing such a file. Powered by nix-index (local installation recommended)."]
        [arg] -> (:[]) <$> doNixLocate Generic arg
        "bin":[arg] -> (:[]) <$> doNixLocate Bin arg
        "man":[arg] -> (:[]) <$> doNixLocate Man arg
        [tp, _] -> return ["Unknown locate type " ++ tp]
        _ -> return [",locate only takes 1 or 2 arguments"]
      "tell":args -> return []
      "find":args -> return []
      [ cmd ] -> replyLookup nick Nothing <$> lookupCommand cmd
      cmd:"=":rest -> case length rest of
          0 -> do
            mvalue <- gets $ M.lookup cmd
            case mvalue of
              Nothing -> return [ cmd ++ " is already undefined" ]
              Just (_, value) -> do
                modify (M.delete cmd)
                return [ "Undefined " ++ cmd ++ ", was defined as: " ++ value]
          _ -> do
            old <- gets $ M.lookup cmd
            modify $ M.insertWith (\(uses, _) (_, new) -> (uses, new)) cmd (0, unwords rest)
            case old of
              Nothing -> return [ cmd ++ " defined" ]
              Just (_, val) -> return [ cmd ++ " redefined, was defined as: " ++ val ]
      cmd:args -> replyLookup nick (Just (unwords args)) <$> lookupCommand cmd
    trans (_, _, _) = return []
