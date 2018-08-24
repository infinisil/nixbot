{-# LANGUAGE FlexibleContexts #-}
module Plugins.Commands (commandsPlugin) where

import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.ByteString.Char8   as BS
import           Data.Char
import           Data.Either
import           Data.Either.Combinators
import           Data.List               (findIndex, intercalate, isSuffixOf,
                                          sortBy)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, fromJust, fromMaybe,
                                          listToMaybe, mapMaybe)
import           Data.Ord                (comparing)
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

lookupCommand :: String -> M.Map String String -> LookupResult
lookupCommand str map = result
  where
    filtered =
      filter ((3 >=) . snd)
      . sortBy (comparing snd)
      . fmap (\s -> (s,
                     levenshteinDistance defaultEditCosts str s)
             )
      . filter (\s -> (s == str) || ((>=3) . length $ s))
      $ if length str <= 3 then (if M.member str map then [str] else []) else M.keys map
    result = case filtered of
               []         -> Empty
               (str, 0):_ -> Exact . fromJust $ M.lookup str map
               (str, _):_ -> Guess str . fromJust $ M.lookup str map

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
    { contents = "with import <nixpkgs> {}; map (path: lib.attrByPath path null pkgs) " ++ nixattrs
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

commandsPlugin :: MonadIO m => MyPlugin (Map String String) m
commandsPlugin = MyPlugin M.empty trans "commands"
  where
    trans (chan, nick, ',':command) = case words command of
      [] -> do
        keys <- gets M.keys
        return ["All commands: " ++ unwords keys]
      "locate":args -> case args of
        [] -> return ["Use ,locate <filename> to find packages containing such a file. Powered by nix-index (local installation recommended)."]
        [arg] -> (:[]) <$> doNixLocate Generic arg
        "bin":[arg] -> (:[]) <$> doNixLocate Bin arg
        "man":[arg] -> (:[]) <$> doNixLocate Man arg
        [tp, _] -> return ["Unknown locate type " ++ tp]
        _ -> return [",locate only takes 1 or 2 arguments"]
      "tell":args -> return []
      "find":args -> return []
      [ cmd ] -> replyLookup nick Nothing <$> gets (lookupCommand cmd)
      cmd:"=":rest -> case length rest of
          0 -> do
            mvalue <- gets $ M.lookup cmd
            case mvalue of
              Nothing -> return [ cmd ++ " is already undefined" ]
              Just value -> do
                modify (M.delete cmd)
                return [ "Undefined " ++ cmd ++ ", was defined as: " ++ value]
          _ -> do
            old <- gets $ M.lookup cmd
            modify (M.insert cmd (unwords rest))
            case old of
              Nothing -> return [ cmd ++ " defined" ]
              Just val -> return [ cmd ++ " redefined, was defined as: " ++ val ]
      cmd:args -> replyLookup nick (Just (unwords args)) <$> gets (lookupCommand cmd)
    trans (_, _, _) = return []
