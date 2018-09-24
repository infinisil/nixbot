module Nix.Session.CanAssign where

import           Control.Monad.Reader

import           Nix
import           System.Process

-- TODO: Add caching

-- | Tests whether a given identifier is assignable by `with` expressions
canAssign :: String -> ReaderT FilePath IO Bool
canAssign var = do
  nixInstantiate <- ask
  stdout <- lift $ filter (/='\n') <$> readProcess nixInstantiate ["--eval", "-"] expr
  return $ str == stdout
  where
    str = "\"hithere\""
    expr = "with { " ++ var ++ " = " ++ str ++ "; }; " ++ var


