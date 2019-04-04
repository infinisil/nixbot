{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Plugins.Commands.Dynamic
  ( Dynamic(..)
  , dynamicParser
  , dynamicHandle
  , listCommands
  ) where

import           Control.Monad.State
import           Data.Aeson
import           Data.Char
import           Data.Functor
import Types
import           Data.List
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Ord
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as TIO
import           Plugins
import           IRC                     hiding (paging)
import           Plugins.Commands.Shared
import           System.Directory
import           System.FilePath
import           Text.EditDistance       (defaultEditCosts, levenshteinDistance)
import           Text.Megaparsec
import           Utils

type Key = Text

data Dynamic = DynamicQuery Key [User]
             | DynamicAssign Key Text
             | DynamicDelete Key
             deriving (Show)

dynamicParser :: Parser Dynamic
dynamicParser = do
  key <- Text.pack . fmap toLower <$> parseWord
  word "=" *> (
      eof $> DynamicDelete key
      <|> DynamicAssign key <$> (Text.pack <$> getInput)
    ) <|> DynamicQuery key <$> many (Text.pack <$> parseWord)

dynamicHandle :: Dynamic -> PluginT App ()
dynamicHandle (DynamicQuery key users) = do
  res <- lookupCommand key
  replies <- replyLookup key users res
  mapM_ reply replies
dynamicHandle (DynamicAssign key value) = do
  answer <- setCommand key value
  reply answer
dynamicHandle (DynamicDelete key) = do
  answer <- delCommand key
  reply answer

getStatFile :: PluginMonad m => m FilePath
getStatFile = (</> "stats.json") <$> getGlobalState

getCommandDir :: PluginMonad m => m FilePath
getCommandDir = (</> "commands") <$> getGlobalState

type Stats = Map Text Int

withStats :: (PluginMonad m, MonadIO m) => StateT Stats m a -> m (Either String a)
withStats action = do
  statFile <- getStatFile
  exists <- liftIO $ doesFileExist statFile
  stats <- if exists then liftIO (eitherDecodeFileStrict statFile) >>= \case
      Left err -> do
        liftIO $ putStrLn $ "Failed to decode command stat file " ++ statFile ++ ": " ++ err
        return $ Left $ "error: Couldn't decode stat file " ++ statFile ++ ": " ++ err
      Right st -> return $ Right st
    else return $ Right Map.empty
  case stats of
    Left err -> return $ Left err
    Right st -> do
      (res, newStats) <- runStateT action st
      liftIO $ encodeFile statFile newStats
      return $ Right res

data LookupResult = Empty | Exact | Guess Text

replyLookup :: Text -> [Text] -> LookupResult -> PluginT App [Text]
replyLookup _ _ Empty = return []
replyLookup cmd arg Exact = do
  val <- unsafeQueryCommand cmd
  return [ case arg of
             []      -> ""
             targets -> Text.unwords targets <> ": "
           <> val
         ]
replyLookup _ arg (Guess cmd) = do
  val <- unsafeQueryCommand cmd
  nick <- getUser
  return [ nick <> ": Did you mean " <> cmd <> "?"
         , case arg of
             []-> ""
             targets -> Text.unwords targets <> ": "
           <> val
         ]

lookupCommand :: (MonadIO m, PluginMonad m) => Text -> m LookupResult
lookupCommand str = withStats (gets Map.keys) >>= \case
  Left _ -> return Empty
  Right cmds -> return $ case result of
    Nothing -> Empty
    Just (0, _) -> Exact
    Just (dist, word') ->
      if fromIntegral dist / fromIntegral (Text.length word') < (0.34 :: Float) then Guess word' else Empty
    where
      str' = Text.unpack str
      assos = map (\k -> (levenshteinDistance defaultEditCosts str' (Text.unpack k), k)) cmds
      result = if null assos then Nothing else Just $ minimumBy (comparing fst) assos

listCommands :: (MonadIO m, PluginMonad m) => [Text] -> Maybe Int -> m Text
listCommands special mpage = do
  res <- withStats $ do
    stats <- get
    let sorted = fmap fst . sortBy (flip $ comparing snd) . Map.assocs $ stats
        pages = paging sorted getPage ircLimit
    return $ if 0 <= page && page < length pages then pages !! page else "Invalid page index, the last page is number " <> Text.pack (show (length pages - 1))
  return $ either Text.pack id res
  where
    getPage 0 items = "Special commands: " <> Text.unwords special
      <> " - Commands sorted by use count, page 0 (use ,<n> to view page <n>): " <> Text.unwords items
    getPage n items = "Page " <> Text.pack (show n) <> ": " <> Text.unwords items
    page = fromMaybe 0 mpage


unsafeQueryCommand :: Text -> PluginT App Text
unsafeQueryCommand cmd = do
  cmdDir <- getCommandDir
  _ <- withStats $ modify $ Map.insertWith (+) cmd 1
  liftIO (TIO.readFile $ cmdDir </> Text.unpack cmd)

setCommand :: (MonadIO m, PluginMonad m) => Text -> Text -> m Text
setCommand cmd val = if not (validCmd cmd) then return "Invalid command key" else do
  file <- (</> Text.unpack cmd) <$> getCommandDir
  exists <- liftIO $ doesFileExist file

  oldValue <- if exists then Just <$> liftIO (TIO.readFile file) else return Nothing
  getCommandDir >>= liftIO . createDirectoryIfMissing True
  liftIO $ TIO.writeFile file val

  _ <- withStats $ modify $ Map.insertWith const cmd 0
  return $ case oldValue of
    Just old -> cmd <> " redefined, was defined as " <> old
    Nothing  -> cmd <> " defined"

validCmd :: Text -> Bool
validCmd (Text.unpack -> str) = str == takeFileName str && isValid str

delCommand :: (MonadIO m, PluginMonad m) => Text -> m Text
delCommand cmd = do
  file <- (</> Text.unpack cmd) <$> getCommandDir
  exists <- liftIO $ doesFileExist file
  if exists then do
    val <- liftIO $ TIO.readFile file
    liftIO $ removeFile file
    _ <- withStats $ modify $ Map.delete cmd
    return $ "Undefined " <> cmd <> ", was defined as: " <> val
  else return $ cmd <> " is already undefined"
