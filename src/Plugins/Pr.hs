{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Pr (prPlugin, Settings(..)) where

import           Plugins

import           Control.Monad             (mzero)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics              (Generic)
import           Text.Regex.TDFA

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status

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

fetchInfo :: MonadIO m => Manager -> ParsedIssue -> m (Maybe Issue)
fetchInfo manager (ParsedIssue _ owner repo number) = do
  response <- liftIO $ httpLbs request manager
  if statusIsSuccessful (responseStatus response)
    then return $ decode (responseBody response)
    else do
      liftIO $ print $ responseBody response
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

displayIssue :: ParsedIssue -> Issue -> String
displayIssue parsed Issue { i_title, i_author, i_state, i_url } =
  let common = " (by " ++ i_author ++ ", " ++ i_state ++ "): " ++ i_title
  in case parsed of
    (ParsedIssue Hash _ _ _)      -> i_url ++ common
    (ParsedIssue Link _ _ number) -> "#" ++ show number ++ common

prReplies :: MonadIO m => Settings -> String -> m [String]
prReplies settings@Settings { prFilter } input = do
  manager <- liftIO $ newManager tlsManagerSettings
  fmap (uncurry displayIssue) . catMaybes . fmap sequence . zip filtered <$> mapM (fetchInfo manager) filtered
  where
    filtered = filter prFilter $ parseIssues settings input

prPlugin :: MonadIO m => Settings -> MyPlugin () m
prPlugin settings = MyPlugin () trans "pr"
  where
    trans (nick, msg) = prReplies settings msg
