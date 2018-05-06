{-# LANGUAGE OverloadedStrings #-}
module Plugins.Nixpkgs ( nixpkgsPlugin
                       ) where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.State             (StateT)
import           Control.Monad.State.Class
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified GitHub.Endpoints.Repos.Contents as R
import           Text.Regex.TDFA                 ((=~))

import           Plugins


parseNixpkgs :: String -> [String]
parseNixpkgs s = map (!! 1) (s =~ ("nixpkgs/([^[:space:]]+)+" :: String))

getNixpkgs :: MonadIO m => String -> m (Maybe String)
getNixpkgs s = do
  liftIO.putStrLn $ "Trying to get contents for " ++ s
  contents <- liftIO $ R.contentsFor "NixOS" "nixpkgs" (Text.pack s) (Just "heads/master")
  case contents of
    Left error -> do
      liftIO $ print error
      return Nothing
    Right contents ->
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/master/" ++ s

nixpkgs :: MonadIO m => String -> m [String]
nixpkgs s = fmap catMaybes . mapM getNixpkgs $ parseNixpkgs s

nixpkgsPlugin :: MonadIO m => MyPlugin () m
nixpkgsPlugin = MyPlugin () trans "nixpkgs"
  where
    trans (nick, msg) = liftIO $ nixpkgs msg

