{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Pr (prPlugin, Settings(..), ParsedIssue(..), ParseType(..)) where

import           Plugins

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List
import qualified Data.Text              as Text
import           Data.Time
import           GitHub                 hiding (Owner, Repo)
import           GitHub.Data.Id
import           GitHub.Data.Name
import           Text.Regex.TDFA
import           Utils

fetchInfo :: (MonadLogger m, MonadIO m) => Settings -> ParsedIssue -> m (Maybe String)
fetchInfo Settings { defOwner, defRepo } (ParsedIssue parseType owner repo number) = do
  logInfoN $ "Fetching info for issue " <> Text.pack owner <> "/" <> Text.pack repo <> "#" <> Text.pack (show number)
  result <- liftIO . executeRequest' $ issueR owner' repo' number'
  case result of
    Left err -> do
      logWarnN $ "Got error from github request (might indicate that this issue doesn't exist): " <> Text.pack (show err)
      return Nothing
    Right issue@Issue { issueHtmlUrl = Nothing } -> do
      logWarnN $ "Issue didn't have an url for some reason: " <> Text.pack (show issue)
      return Nothing
    Right issue@Issue { issueHtmlUrl = Just url } -> Just <$> do
      state <- case (issuePullRequest issue, issueState issue) of
        (Just _, StateClosed) -> do
          logInfoN "This issue is a PR and closed, fetching to see if it's merged"
          pullResult <- liftIO $ executeRequest' $ pullRequestR owner' repo' number'
          return $ case pullResult of
            Right PullRequest { pullRequestMerged = True } -> "merged"
            _                                              -> showState
        _ -> return showState
      ago <- liftIO $ fmap (prettySeconds 1 . round . (`diffUTCTime` issueCreatedAt issue)) getCurrentTime
      return $ prefix ++ " (by " ++ author ++ ", " ++ ago ++ " ago, " ++ state ++ "): " ++ title
      where
        author = Text.unpack . untagName . simpleUserLogin . issueUser $ issue
        title = Text.unpack . issueTitle $ issue
        prefix = case parseType of
          Hash -> Text.unpack . getUrl $ url
          Link -> case (defOwner repo == owner, defRepo == repo) of
            (False, _)    -> owner ++ "/" ++ repo ++ "#" ++ show number
            (True, False) -> repo ++ "#" ++ show number
            (True, True)  -> "#" ++ show number
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

sameIssue :: ParsedIssue -> ParsedIssue -> Bool
sameIssue (ParsedIssue _ owner1 repo1 num1) (ParsedIssue _ owner2 repo2 num2) =
  owner1 == owner2 && repo1 == repo2 && num1 == num2

parseIssues :: Settings -> String -> [ParsedIssue]
parseIssues Settings { defOwner, defRepo } = map extractIssue . match prRegex
  where
    prRegex :: RegexMaker a CompOption ExecOption String => a
    prRegex = makeRegex $ "(([^ ()]+)/)?([^ ()]+)?#([[:digit:]]+)"
      ++ "|" ++ "https://github.com/([^/ ]+)/([^/ ]+)/(issues|pull)/([[:digit:]]+)([^#/[:digit:]]|\\')"

    extractIssue :: [String] -> ParsedIssue
    extractIssue [_, _, _, _, "", owner, repo, _, numberStr, _] =
      ParsedIssue Link owner repo (read numberStr)
    extractIssue [_, _, owner, repo, numberStr, _, _, _, _, _]
      | null repo = ParsedIssue Hash (defOwner defRepo) defRepo number
      | null owner = ParsedIssue Hash (defOwner repo) repo number
      | otherwise = ParsedIssue Hash owner repo number
      where number = read numberStr
    extractIssue m = error $ "The pull request regex has a bug, the following part was matched but not caught by any patterns: " ++ head m

data Settings = Settings
  { defOwner :: Repo -> Owner
  , defRepo  :: Repo
  , prFilter :: ParsedIssue -> Bool
  }



prPlugin :: Settings -> Plugin
prPlugin settings = Plugin
  { pluginName = "pr"
  , pluginCatcher = \Input { inputMessage } -> case filter (prFilter settings) . nubBy sameIssue $ parseIssues settings inputMessage of
      []     -> PassedOn
      result -> Consumed result
  , pluginHandler = \result ->
      forM_ result $ fetchInfo settings >=> \case
        Nothing -> return ()
        Just msg -> reply msg
  }
