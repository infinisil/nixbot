{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Nixpkgs ( nixpkgsPlugin
                       ) where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.State             (StateT)
import           Control.Monad.State.Class
import           Data.List
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as TIO
import qualified GitHub                          as GH
import           GitHub.Data.Name
import qualified GitHub.Endpoints.Repos.Commits  as C
import qualified GitHub.Endpoints.Repos.Contents as R
import           Text.Regex.TDFA                 ((=~))

import           Data.Monoid                     ((<>))
import           Nixpkgs
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
    Right GH.Commit { GH.commitSha = N sha } ->
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/" <> Text.take 7 sha <> s

findPath :: (Monad m, MonadNixpkgs m) => Text -> m (Maybe Text)
findPath input = do
  let actualInput = reverse $ Text.split (=='/') input
  NixpkgsState { fileIndex } <- nixpkgsState
  let result = find (isPrefixOf actualInput) (map fst fileIndex)
  --find (Text.isSuffixOf input) cache
  return $ Text.intercalate "/" . reverse <$> result

prependSlash :: Text -> Text
prependSlash = ("/"<>) . snd . Text.break (/='/')


tryNixpkgs :: (MonadNixpkgs m, Monad m) => String -> m (Maybe String)
tryNixpkgs input = do
  result <- findPath (Text.pack input)
  return $ Text.unpack <$> result

nixpkgs :: (MonadNixpkgs m, MonadIO m) => String -> m [String]
nixpkgs s = do
  found <- catMaybes <$> mapM tryNixpkgs (parseNixpkgs s)
  NixpkgsState { master = Commit { sha } } <- nixpkgsState
  return $ map (\file -> "https://github.com/NixOS/nixpkgs/tree/" ++ take 7 sha ++ "/" ++ file) found

nixpkgsPlugin :: (MonadNixpkgs m, MonadIO m) => MyPlugin () m
nixpkgsPlugin = MyPlugin () trans "nixpkgs"
  where
    trans (chan, nick, msg) = nixpkgs msg

