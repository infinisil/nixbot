{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugins.Commands.RandomPr where

import           Config
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Types          as AT
import qualified Data.ByteString           as BS
import qualified Data.HashMap.Strict       as H
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Encoding
import           Data.Time
import qualified Data.Vector               as V
import           NeatInterpolation
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           Plugins
import           System.Random
import           Types
import           Utils

baseRequest :: Request
baseRequest = parseRequest_ "https://api.github.com/graphql"

prCountQuery :: Text
prCountQuery = [text|
  {
          repository(name: "nixpkgs", owner: "NixOS") {
                  pullRequests(last: 1, states: OPEN) {
                          nodes {
                                  number
                          }
                  }
          }
  }
  |]

prCountParser :: Object -> AT.Parser Int
prCountParser obj = do
  ddata <- obj .: "data"
  repo <- ddata .: "repository"
  prs <- repo .: "pullRequests"
  nodes :: Array <- prs .: "nodes"
  Object nodeObj <- return $ nodes V.! 0
  nodeObj .: "number"

openPrsQuery :: Int -> IO Text
openPrsQuery maxNum = do
  nums <- randomNumbers maxNum 100
  return $ "{ repository(name: \"nixpkgs\", owner: \"NixOS\") {" <> Text.concat (map genSub nums) <> "} }"
  where
    genSub :: Int -> Text
    genSub n = "n" <> Text.pack (show n) <> ": issueOrPullRequest(number: " <> Text.pack (show n) <> ") { ... on PullRequest { url state author { login } createdAt title mergeable isDraft } }"

openPrsParser :: UTCTime -> Object -> AT.Parser (Maybe Text)
openPrsParser now obj = do
  ddata <- obj .: "data"
  repo :: Object <- ddata .: "repository"
  let prs = H.elems repo
  t <- traverse openPrText prs
  return $ listToMaybe $ catMaybes t
  where
    openPrText :: Value -> AT.Parser (Maybe Text)
    openPrText (Object obj') = do
      state :: Maybe Text <- obj' .:? "state"
      case state of
        Just "OPEN" -> do
          url <- obj' .: "url"
          author <- obj' .: "author"
          login :: Text <- author .: "login"
          createdAt :: UTCTime <- obj' .: "createdAt"
          title :: Text <- obj' .: "title"
          mergeable :: Text <- obj' .: "mergeable"
          isDraft :: Bool <- obj' .: "isDraft"
          let wipInTitle = "wip" `elem` Text.words (Text.toLower title)
              isWIP = isDraft || wipInTitle
              ago = prettySeconds 1 . round . (`diffUTCTime` createdAt) $ now
          return $ case (mergeable, isWIP) of
            ("CONFLICTING", _) -> Nothing
            (_, True) -> Nothing
            (_, False) -> Just $ "[development] " <> url <> " (by " <> login <> ", " <> ago <> " ago, open): " <> title
        _ -> return Nothing
    openPrText _ = return Nothing


makeGraphQLRequest :: Manager -> BS.ByteString -> Text -> IO Object
makeGraphQLRequest mng token query = do
  let jsonData :: Object = "query" .= String query
  let requestData = encode jsonData
  let req = baseRequest
        { requestBody = RequestBodyLBS requestData
        , requestHeaders =
          [ ("Authorization", "token " <> token)
          , ("User-Agent", "haskell")
          ]
        , method = "POST"
        }
  resp <- httpLbs req mng
  if statusIsSuccessful (responseStatus resp)
    then return $ fromJust (decode (responseBody resp))
    else fail $ "Error status code " <> show (responseStatus resp) <> " for graphql query, response body is: " <> show (responseBody resp)

randomNumbers :: Int -> Int -> IO [Int]
randomNumbers _ 0 = return []
randomNumbers maxNum count = do
  num <- randomRIO (0, maxNum)
  rest <- randomNumbers maxNum (count - 1)
  return (num : rest)

getOpenPR :: Manager -> BS.ByteString -> Int -> IO (Either String Text)
getOpenPR mng token maxNum = do
  query <- openPrsQuery maxNum
  result <- makeGraphQLRequest mng token query
  now <- getCurrentTime
  case AT.parseEither (openPrsParser now) result of
    Left err         -> return $ Left err
    Right Nothing    -> getOpenPR mng token maxNum
    Right (Just txt) -> return $ Right txt

getRandomPR :: BS.ByteString -> IO (Either String Text)
getRandomPR token = do
  mng <- liftIO $ newManager tlsManagerSettings

  result <- liftIO $ makeGraphQLRequest mng token prCountQuery
  let Just count = AT.parseMaybe prCountParser result

  liftIO $ getOpenPR mng token count


randomPrHandle :: PluginT App ()
randomPrHandle = do
  sender <- getSender
  token <- lift (asks (encodeUtf8 . configToken . configRandomPr . configCommands . pluginConfigForSender sender . config))
  liftIO (getRandomPR token) >>= \case
    Left err -> reply (Text.pack err)
    Right txt -> reply txt
