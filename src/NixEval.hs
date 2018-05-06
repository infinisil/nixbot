{-# LANGUAGE NamedFieldPuns #-}
module NixEval ( nixInstantiate
               , NixOptions(..)
               , EvalMode(..)
               , publicOptions
               , def
               , NixEvalOptions(..)
               ) where

import           Control.Monad.IO.Class
import           Data.Default
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe
import           System.Exit
import           System.Process

data NixOptions = NixOptions
  { cores                     :: Int
  , fsyncMetadata             :: Bool
  , restrictEval              :: Bool
  , sandbox                   :: Bool
  , timeout                   :: Int
  , maxJobs                   :: Int
  , allowImportFromDerivation :: Bool
  , allowedUris               :: [String]
  , showTrace                 :: Bool
  }
  deriving (Show)

instance Default NixOptions where
  def = NixOptions
    { cores = 1
    , fsyncMetadata = True
    , restrictEval = False
    , sandbox = False
    , timeout = 0
    , maxJobs = 1
    , allowImportFromDerivation = True
    , allowedUris = []
    , showTrace = True
    }

publicOptions :: NixOptions
publicOptions = def
  { cores = 0
  , fsyncMetadata = False
  , restrictEval = True
  , sandbox = True
  , timeout = 3
  , maxJobs = 0
  , allowImportFromDerivation = False
  }

optionsToArgs :: NixOptions -> [String]
optionsToArgs NixOptions
  { cores
  , fsyncMetadata
  , restrictEval
  , sandbox
  , timeout
  , maxJobs
  , allowImportFromDerivation
  , allowedUris
  , showTrace
  } = concat [ [ "--option", name, value ] | (name, value) <-
        [ ("cores", show cores)
        , ("fsync-metadata", bool fsyncMetadata)
        , ("restrict-eval", bool restrictEval)
        , ("sandbox", bool sandbox)
        , ("timeout", show timeout)
        , ("max-jobs", show maxJobs)
        , ("allow-import-from-derivation", bool allowImportFromDerivation)
        , ("allowed-uris", unwords allowedUris)
        , ("show-trace", bool showTrace)
        ] ]
      where
        bool True  = "true"
        bool False = "false"

data EvalMode = Parse | Lazy | Strict | Json

modeToArgs :: EvalMode -> [String]
modeToArgs Parse  = ["--parse"]
modeToArgs Lazy   = ["--eval"]
modeToArgs Strict = ["--eval", "--strict"]
modeToArgs Json   = ["--eval", "--strict", "--json"]

data NixEvalOptions = NixEvalOptions
  { contents  :: String
  , attribute :: Maybe String
  , arguments :: Map String String
  , nixPath   :: [String]
  , mode      :: EvalMode
  , options   :: NixOptions
  }

instance Default NixEvalOptions where
  def = NixEvalOptions
    { contents = ""
    , attribute = Nothing
    , arguments = M.empty
    , nixPath = []
    , mode = Lazy
    , options = def
    }

nixInstantiatePath = "/run/current-system/sw/bin/nix-instantiate"

nixInstantiate :: MonadIO m => NixEvalOptions -> m (Either String String)
nixInstantiate NixEvalOptions { contents, attribute, arguments, nixPath, mode, options } = do
  liftIO . putStrLn $ "Calling nix-instantiate with options:\n" ++ concatMap (\o -> "\t" ++ o ++ "\n") opts ++ "\nwith file:\n" ++ contents ++ "\n"
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode nixInstantiatePath opts contents
  case exitCode of
    ExitSuccess      -> return . Right $ outputTransform stdout
    ExitFailure code -> return . Left $ outputTransform stderr
  where
    opts = modeToArgs mode
      ++ [ "-" ]
      ++ maybe [] (\a -> [ "-A", a ]) attribute
      ++ concatMap (\(var, val) -> [ "--arg", var, val ]) (M.assocs arguments)
      ++ concatMap (\p -> [ "-I", p ]) nixPath
      ++ optionsToArgs options

outputTransform :: String -> String
outputTransform = take 200 . fromMaybe "(no output)" . listToMaybe . take 1 . reverse . lines
