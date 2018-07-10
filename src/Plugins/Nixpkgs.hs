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
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/" <> Text.take 7 sha <> "/" <> s

initCache :: MonadIO m => m [Text]
initCache = liftIO $ Text.lines <$> TIO.readFile "filecache"

findPath :: [Text] -> Text -> Maybe Text
findPath cache input = find (Text.isSuffixOf input) cache

nixpkgs :: MonadIO m => [Text] -> String -> m [String]
nixpkgs cache s = do
  let searches = mapMaybe (findPath cache . Text.pack) $ parseNixpkgs s
  results <- catMaybes <$> mapM getNixpkgs searches
  return $ map Text.unpack results

nixpkgsPlugin :: MonadIO m => [Text] -> MyPlugin () m
nixpkgsPlugin cache = MyPlugin () trans "nixpkgs"
  where
    trans (nick, msg) = nixpkgs cache msg

