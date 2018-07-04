{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Pr (prPlugin) where

import           Plugins

import           Control.Monad          (mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, Value (..), decode,
                                         defaultOptions, encode,
                                         genericParseJSON, genericToEncoding,
                                         parseJSON, toEncoding, (.:))
import           Data.Maybe             (catMaybes)
import           GHC.Generics           (Generic)
import qualified Network.HTTP.Simple    as H
import           Text.Regex.TDFA

data Issue = Issue
  { i_title  :: String
  , i_state  :: String
  , i_url    :: String
  , i_author :: String
  } deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON (Object v) = do
    title <- v .: "title"
    state <- v .: "state"
    url <- v .: "html_url"
    user <- v .: "user"
    case user of
      Object o -> Issue title state url <$> o.: "login"
      _        -> mzero

  parseJSON _ = mzero

prToInfo :: (String, Int) -> IO (Maybe String)
prToInfo (p, n) = do
  putStrLn $ "Sending request to github for nixOS/" ++ p ++ "#" ++ show n

  req <- fmap (H.setRequestHeader "user-agent" ["haskell"]) $ H.parseRequest $ "https://api.github.com/repos/nixOS/" ++ p ++ "/issues/" ++ show n
  body <- H.getResponseBody <$> H.httpJSONEither req
  case body of
    Right result -> return $ Just $ i_url result ++ " (by " ++ i_author result ++ ", " ++ i_state result ++ "): " ++ i_title result
    Left _ -> return Nothing


type Owner = String
type Repo = String
data ParseType = Hash | Link deriving Show

data ParsedIssue = ParsedIssue ParseType Owner Repo Int deriving Show

parseIssues :: Repo -> (Repo -> Owner) -> String -> [ParsedIssue]
parseIssues defRep defOwner = map extract . match prRegex
  where
    prRegex :: RegexMaker a CompOption ExecOption String => a
    prRegex = makeRegex $ "(([^ ]+)/)?([^ ]+)?#([[:digit:]]+)"
      ++ "|" ++ "https://github.com/([^/ ]+)/([^/ ]+)/(issues|pull)/([[:digit:]]+)"

    extract :: [String] -> ParsedIssue
    extract [full, _, _, _, "", owner, repo, _, numberStr] =
      ParsedIssue Link owner repo (read numberStr)
    extract [full, _, owner, repo, numberStr, _, _, _, _]
      | null repo = ParsedIssue Hash (defOwner defRep) defRep number
      | null owner = ParsedIssue Hash (defOwner repo) repo number
      | otherwise = ParsedIssue Hash owner repo number
      where number = read numberStr

parsePRs :: String -> String -> [(String, Int)]
parsePRs def str = parsed
  where
    matches :: [[String]]
    matches = str =~ ("([[:alpha:]]+)?#([[:digit:]]+)" :: String)
    parsed :: [(String, Int)]
    parsed = map (\(a, b) -> (if null a then def else a, b)) . map (\xs -> (xs !! 1, read $ xs !! 2)) $ matches

prPlugin :: MonadIO m => MyPlugin () m
prPlugin = MyPlugin () trans "pr"
  where
    trans (nick, msg) = liftIO $ fmap catMaybes . mapM prToInfo . filter (\(repo, n) -> repo /= "nixpkgs" || n >= 1000). parsePRs "nixpkgs" $ msg
