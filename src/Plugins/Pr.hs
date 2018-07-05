{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Plugins.Pr (prPlugin, Settings(..), ParsedIssue(..), ParseType(..)) where

import           Plugins

import           Control.Monad             (mzero)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.HashMap.Strict       as H
import           Data.Maybe
import           GHC.Generics              (Generic)
import           Text.Regex.TDFA

import qualified Data.Text                 as Text

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status

import           GitHub                    hiding (Owner, Repo)
import           GitHub.Data.Id
import           GitHub.Data.Name

data IssueInfo = IssueInfo
  { issue_state  :: String
  , issue_url    :: String
  , issue_author :: String
  , issue_title  :: String
  } deriving Show

fetchInfo :: (MonadLogger m, MonadIO m) => Manager -> ParsedIssue -> m (Maybe String)
fetchInfo manager pissue@(ParsedIssue parseType owner repo number) = do
  $(logInfoSH)$ "Fetching info for issue " ++ owner ++ "/" ++ repo ++ "#" ++ show number
  result <- liftIO . executeRequest' $ issueR owner' repo' number'
  case result of
    Left error -> do
      $(logWarnSH)$ "Got error from github request (might indicate that this issue doesn't exist): " ++ show error
      return Nothing
    Right issue@Issue { issueHtmlUrl = Nothing } -> do
      $(logWarnSH)$ "Issue didn't have an url for some reason: " ++ show issue
      return Nothing
    Right issue@Issue { issueHtmlUrl = Just url } -> Just <$> do
      state <- case (issuePullRequest issue, issueState issue) of
        (Just _, StateClosed) -> do
          $(logInfoSH)$ "This issue is a PR and closed, fetching to see if it's merged"
          pullResult <- liftIO $ executeRequest' $ pullRequestR owner' repo' number'
          return $ case pullResult of
            Right PullRequest { pullRequestMerged = True } -> "merged"
            _                                              -> showState
        _ -> return showState
      return $ prefix ++ " (by " ++ author ++ ", " ++ state ++ "): " ++ title
      where
        author = Text.unpack . untagName . simpleUserLogin . issueUser $ issue
        title = Text.unpack . issueTitle $ issue
        prefix = case parseType of
          Hash -> Text.unpack . getUrl $ url
          Link -> "#" ++ show number
        showState = case issueState issue of
          StateClosed -> "closed"
          StateOpen   -> "open"
  where
    owner' = N $ Text.pack owner
    repo' = N $ Text.pack repo
    number' = Id number

type Owner = String
type Repo = String
data ParseType = Hash | Link deriving Show

data ParsedIssue = ParsedIssue ParseType Owner Repo Int deriving Show

parseIssues :: Settings -> String -> [ParsedIssue]
parseIssues Settings { defOwner, defRepo } = map extract . match prRegex
  where
    prRegex :: RegexMaker a CompOption ExecOption String => a
    prRegex = makeRegex $ "(([^ ]+)/)?([^ ]+)?#([[:digit:]]+)"
      ++ "|" ++ "https://github.com/([^/ ]+)/([^/ ]+)/(issues|pull)/([[:digit:]]+)"

    extract :: [String] -> ParsedIssue
    extract [full, _, _, _, "", owner, repo, _, numberStr] =
      ParsedIssue Link owner repo (read numberStr)
    extract [full, _, owner, repo, numberStr, _, _, _, _]
      | null repo = ParsedIssue Hash (defOwner defRepo) defRepo number
      | null owner = ParsedIssue Hash (defOwner repo) repo number
      | otherwise = ParsedIssue Hash owner repo number
      where number = read numberStr

data Settings = Settings
  { defOwner :: Repo -> Owner
  , defRepo  :: Repo
  , prFilter :: ParsedIssue -> Bool
  }


prReplies :: (MonadLogger m, MonadIO m) => Settings -> String -> m [String]
prReplies settings@Settings { prFilter } input = do
  manager <- liftIO $ newManager tlsManagerSettings
  catMaybes <$> mapM (fetchInfo manager) filtered
  where
    filtered = filter prFilter $ parseIssues settings input

prPlugin :: (MonadLogger m, MonadIO m) => Settings -> MyPlugin () m
prPlugin settings = MyPlugin () trans "pr"
  where
    trans (nick, msg) = prReplies settings msg
