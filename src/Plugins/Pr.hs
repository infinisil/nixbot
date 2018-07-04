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

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status

data Pull = Pull
  { p_merged :: Bool
  } deriving (Show, Generic)

data Issue = Issue
  { i_pr     :: Maybe String
  , i_title  :: String
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
      Object o -> do
        pr <- if H.member "pull_request" v then do
          pull_request <- v .: "pull_request"
          case pull_request of
            Object x -> Just <$> x .: "url"
            _        -> return Nothing

          else return Nothing
        Issue pr title state url <$> o.: "login"
      _        -> mzero

  parseJSON _ = mzero

instance FromJSON Pull where
  parseJSON (Object v) = Pull <$> v .: "merged"

data IssueInfo = IssueInfo
  { issue_state  :: String
  , issue_url    :: String
  , issue_author :: String
  , issue_title  :: String
  } deriving Show

fetchInfo :: (MonadLogger m, MonadIO m) => Manager -> ParsedIssue -> m (Maybe IssueInfo)
fetchInfo manager (ParsedIssue _ owner repo number) = do
  $(logInfoSH)$ "Making request to " ++ show request
  response <- liftIO $ httpLbs request manager
  if statusIsSuccessful (responseStatus response)
    then case decode (responseBody response) of
      Nothing -> return Nothing
      Just issue@Issue { i_state, i_url, i_author, i_title, i_pr } -> do
        $(logDebugSH)$ issue
        case i_pr of
          Nothing -> return $ Just $ defaultIssue
          Just url -> do
            $(logDebugSH)$ "Making request to " ++ show url
            pullResponse <- liftIO $ httpLbs ((parseRequest_ url) { requestHeaders = [("user-agent", "haskell")] }) manager
            $(logDebugSH)$ "Got body: " ++ show (responseBody pullResponse)
            case decode (responseBody pullResponse) :: Maybe Pull of
              Just Pull { p_merged = True } -> return $ Just $ defaultIssue
                { issue_state = "merged"
                }
              _ -> return $ Just $ defaultIssue
        where
          defaultIssue = IssueInfo
            { issue_state = i_state
            , issue_url = i_url
            , issue_author = i_author
            , issue_title = i_title
            }

    else do
      $(logErrorSH)$ "Failed to get info, error: " ++ show (responseBody response)
      return Nothing
  where
    request :: Request
    request = (parseRequest_ $ "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/issues/" ++ show number)
      { requestHeaders = [("user-agent", "haskell")] }

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

displayIssue :: ParsedIssue -> IssueInfo -> String
displayIssue parsed IssueInfo { issue_title, issue_author, issue_state, issue_url } =
  let common = " (by " ++ issue_author ++ ", " ++ issue_state ++ "): " ++ issue_title
  in case parsed of
    (ParsedIssue Hash _ _ _)      -> issue_url ++ common
    (ParsedIssue Link _ _ number) -> "#" ++ show number ++ common

prReplies :: (MonadLogger m, MonadIO m) => Settings -> String -> m [String]
prReplies settings@Settings { prFilter } input = do
  manager <- liftIO $ newManager tlsManagerSettings
  fmap (uncurry displayIssue) . catMaybes . fmap sequence . zip filtered <$> mapM (fetchInfo manager) filtered
  where
    filtered = filter prFilter $ parseIssues settings input

prPlugin :: (MonadLogger m, MonadIO m) => Settings -> MyPlugin () m
prPlugin settings = MyPlugin () trans "pr"
  where
    trans (nick, msg) = prReplies settings msg
