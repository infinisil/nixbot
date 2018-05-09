{-# LANGUAGE NamedFieldPuns #-}
module NixEval ( nixInstantiate
               , NixOptions(..)
               , EvalMode(..)
               , publicOptions
               , def
               , NixEvalOptions(..)
               ) where

import           Control.Monad.IO.Class
import           Data.Char
import           Data.Default
import           Data.Function          (on)
import           Data.List
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Word
import           System.Exit
import           System.Process
import           Text.Megaparsec        as P
import           Text.Megaparsec.Char   as C
import           Text.Read

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
  liftIO . putStrLn $ "Got on stdout:\n" ++ stdout ++ "\n"
  liftIO . putStrLn $ "Got on stderr:\n" ++ stderr ++ "\n"
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

data Command = Command [Int] Char deriving Show

getNumbers :: String -> [Int]
getNumbers str = getNum groups
  where
    groups = groupBy (\a b -> isNumber a && isNumber b) str
    getNum :: [String] -> [Int]
    getNum []           = []
    getNum [num]        = [read num]
    getNum (num:_:rest) = read num : getNum rest

trans :: String -> [Either Command Char]
trans ('\ESC':'[':rest) = case right of
  (c:r) -> Left (Command (getNumbers codes) c) : trans r
  []    -> []
  where
    (codes, right) = flip break rest $
      \s -> not (s == ';' || generalCategory s == DecimalNumber)
    getNumbers = getNum . groups
    groups = groupBy ((&&) `on` isNumber)
    getNum []           = []
    getNum [num]        = [read num]
    getNum (num:_:rest) = read num : getNum rest
trans (c:rest)      = Right c : trans rest
trans [] = []

-- From makefu:
-- https://github.com/krebscode/Reaktor/blob/139a1116c35f57888853ebb3ff585e28921780ff/reaktor/translate_colors.py
toIRCCodes :: Map Int String
toIRCCodes = M.fromList
  [ (0, "\SI")
  , (37, "\ETX00")
  , (30, "\ETX01")
  , (34, "\ETX02")
  , (32, "\ETX03")
  , (31, "\ETX04")
  , (33, "\ETX05")
  , (35, "\ETX06")
  , (33, "\ETX07")
  , (33, "\ETX08")
  , (32, "\ETX09")
  , (36, "\ETX10")
  , (36, "\ETX11")
  , (34, "\ETX12")
  , (31, "\ETX13")
  , (30, "\ETX14")
  , (37, "\ETX15")
  , (1, "\STX")
  , (22, "\STX")
  ]

toIRC :: Command -> String
toIRC (Command [] 'm')   = ""
toIRC (Command ints 'm') = concatMap (\i -> M.findWithDefault "" i toIRCCodes) ints
toIRC _                  = ""

translateCodes (Left cmd)   = toIRC cmd
translateCodes (Right char) = [char]

takeChars :: Int -> [Either Command Char] -> [Either Command Char]
takeChars n []                    = []
takeChars 0 _                     = []
takeChars n (cmd@(Left _):rest)   = cmd : takeChars n rest
takeChars n (char@(Right _):rest) = char : takeChars (n - 1) rest

outputTransform :: String -> String
outputTransform = concatMap translateCodes . takeChars 200 . trans . fromMaybe "(no output)" . listToMaybe . take 1 . reverse . lines
