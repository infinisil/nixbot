{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module NixEval ( nixInstantiate
               , NixOptions(..)
               , unsetNixOptions
               , EvalMode(..)
               , NixEvalOptions(..)
               , defNixEvalOptions
               , outputTransform
               ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.Default
import           Data.Function        (on)
import           Data.List
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Word
import           System.Exit
import           System.Process
import qualified System.Process.Typed as TP
import           Text.Megaparsec      as P
import           Text.Megaparsec.Char as C
import           Text.Read

data NixOptions = NixOptions
  { cores                     :: Maybe Int
  , fsyncMetadata             :: Maybe Bool
  , restrictEval              :: Maybe Bool
  , sandbox                   :: Maybe Bool
  , timeout                   :: Maybe Int
  , maxJobs                   :: Maybe Int
  , allowImportFromDerivation :: Maybe Bool
  , allowedUris               :: Maybe [String]
  , showTrace                 :: Maybe Bool
  }
  deriving (Show)

unsetNixOptions = NixOptions
  { cores = Nothing
  , fsyncMetadata = Nothing
  , restrictEval = Nothing
  , sandbox = Nothing
  , timeout = Nothing
  , maxJobs = Nothing
  , allowImportFromDerivation = Nothing
  , allowedUris = Nothing
  , showTrace = Nothing
  }
{-
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
-}

optionsToArgs :: NixOptions -> [String]
optionsToArgs opts = concat
  [ opt "cores" cores show
  , opt "fsync-metadata" fsyncMetadata bool
  , opt "restrict-eval" restrictEval bool
  , opt "sandbox" sandbox bool
  , opt "timeout" timeout show
  , opt "max-jobs" maxJobs show
  , opt "allow-import-from-derivation" allowImportFromDerivation bool
  , opt "allowed-uris" allowedUris unwords
  , opt "show-trace" showTrace bool
  ] where
    opt :: String -> (NixOptions -> Maybe a) -> (a -> String) -> [String]
    opt name get toStr = case get opts of
      Nothing    -> []
      Just value -> [ "--option", name, toStr value ]
    bool True  = "true"
    bool False = "false"

data EvalMode = Parse | Lazy | Strict | Json

modeToArgs :: EvalMode -> [String]
modeToArgs Parse  = ["--parse"]
modeToArgs Lazy   = ["--eval"]
modeToArgs Strict = ["--eval", "--strict"]
modeToArgs Json   = ["--eval", "--strict", "--json"]

data NixEvalOptions = NixEvalOptions
  { contents   :: Either ByteString FilePath
  , attributes :: [String]
  , arguments  :: Map String String
  , nixPath    :: [String]
  , mode       :: EvalMode
  , options    :: NixOptions
  }

defNixEvalOptions :: Either ByteString FilePath -> NixEvalOptions
defNixEvalOptions file = NixEvalOptions
  { contents = file
  , attributes = []
  , arguments = M.empty
  , nixPath = []
  , mode = Lazy
  , options = unsetNixOptions
  }

toProc :: FilePath -> NixEvalOptions -> TP.ProcessConfig () () ()
toProc nixInstantiatePath NixEvalOptions { contents, attributes, arguments, nixPath, mode, options } = let
  opts = modeToArgs mode
    ++ [case contents of
          Left _     -> "-"
          Right path -> path
       ]
    ++ concatMap (\a -> [ "-A", a ]) attributes
    ++ concatMap (\(var, val) -> [ "--arg", var, val ]) (M.assocs arguments)
    ++ concatMap (\p -> [ "-I", p ]) nixPath
    ++ optionsToArgs options
  process = TP.proc nixInstantiatePath opts
  in case contents of
    Left bytes -> TP.setStdin (TP.byteStringInput bytes) process
    Right _    -> process


nixInstantiate :: FilePath -> NixEvalOptions -> IO (Either ByteString ByteString)
nixInstantiate nixInstPath opts = toEither <$> TP.readProcess (toProc nixInstPath opts)
  where toEither (ExitSuccess, stdout, _)   = Right stdout
        toEither (ExitFailure _, _, stderr) = Left stderr





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

limit :: Int -> [Either Command Char] -> [Either Command Char]
limit 0 []                = []
limit 0 _                 = fmap Right "..."
limit n []                = []
limit n (Left cmd:rest)   = Left cmd : limit n rest
limit n (Right char:rest) = Right char : limit (n-1) rest

outputTransform :: String -> String
outputTransform = concatMap translateCodes . limit 200 . trans . fromMaybe "(no output)" . listToMaybe . take 1 . reverse . lines
