{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Nixpkgs ( nixpkgsPlugin
                       ) where

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.List
import           Data.Maybe
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           GitHub
import           GitHub.Data.Name
import qualified GitHub.Endpoints.Repos.Commits as C
import           Plugins


getNixpkgs :: MonadIO m => Text -> m (Maybe Text)
getNixpkgs s = do
  c <- liftIO $ C.commit "NixOS" "nixpkgs" "HEAD"
  case c of
    Left err -> do
      liftIO $ print err
      return Nothing
    Right Commit { commitSha = N sha } ->
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/" <> Text.take 8 sha <> s

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
    trans (_, _, msg) = map Text.unpack <$> case words msg of
      [",find"] -> return ["Use ,find to get a GitHub link to the master version of files in nixpkgs, e.g. \",find hello/default.nix\" or \",find all-packages.nix\""]
      ",find":args -> do
        ree <- nixpkgs cache args
        case ree of
          []      -> return ["Couldn't find any such files"]
          results -> return $ map ("Found file: " <>) results
      _ -> return []

