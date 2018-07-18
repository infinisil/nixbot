{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Nixpkgs ( nixpkgsPlugin
                       ) where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.State             (StateT)
import           Control.Monad.State.Class
import           Data.List
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as TIO
import           GitHub
import           GitHub.Data.Name
import qualified GitHub.Endpoints.Repos.Commits  as C
import qualified GitHub.Endpoints.Repos.Contents as R
import           Text.Regex.TDFA                 ((=~))

import           Data.Monoid                     ((<>))
import           Plugins


parseNixpkgs :: String -> [String]
parseNixpkgs s = map (!! 1) (s =~ ("<([^ <>]+)>" :: String))

getNixpkgs :: MonadIO m => Text -> m (Maybe Text)
getNixpkgs s = do
  c <- liftIO $ C.commit "NixOS" "nixpkgs" "HEAD"
  case c of
    Left error -> do
      liftIO $ print error
      return Nothing
    Right Commit { commitSha = N sha } ->
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/" <> Text.take 7 sha <> s

initCache :: MonadIO m => m [Text]
initCache = liftIO $ Text.lines <$> TIO.readFile "filecache"

findPath :: [Text] -> Text -> Maybe Text
findPath cache input = find (Text.isSuffixOf input) cache

prependSlash :: Text -> Text
prependSlash = ("/"<>) . snd . Text.break (/='/')

nixpkgs :: MonadIO m => [Text] -> [String] -> m [Text]
nixpkgs cache s = do
  let searches = mapMaybe (findPath cache . prependSlash . Text.pack) s
  catMaybes <$> mapM getNixpkgs searches

nixpkgsPlugin :: MonadIO m => [Text] -> MyPlugin () m
nixpkgsPlugin cache = MyPlugin () trans "nixpkgs"
  where
    trans (chan, nick, msg) = map Text.unpack <$> case words msg of
      [",find"] -> return ["Use ,find to get a GitHub link to the master version of files in nixpkgs, e.g. \",find hello/default.nix\" or \",find all-packages.nix\""]
      ",find":args -> do
        ree <- nixpkgs cache args
        case ree of
          []      -> return ["Couldn't find any such files"]
          results -> return $ map ("Found file: " <>) results
      _ -> return []

