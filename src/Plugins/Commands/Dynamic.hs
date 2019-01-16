{-# LANGUAGE LambdaCase #-}

module Plugins.Commands.Dynamic
  ( Dynamic
  , dynamicParser
  , dynamicHandle
  ) where

import           Control.Monad.State
import           Data.Aeson
import           Data.Functor
import           Data.List
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Ord
import           IRC
import           Plugins
import           Plugins.Commands.Shared
import           System.Directory
import           System.FilePath
import qualified System.IO.Strict        as SIO
import           Text.EditDistance       (defaultEditCosts, levenshteinDistance)
import           Text.Megaparsec

type Key = String

data Dynamic = DynamicQuery Key [User]
             | DynamicAssign Key String
             | DynamicDelete Key
             deriving (Show)

dynamicParser :: Parser Dynamic
dynamicParser = do
  key <- parseWord
  word "=" *> (
      eof $> DynamicDelete key
      <|> DynamicAssign key <$> getInput
    ) <|> DynamicQuery key <$> many parseWord

dynamicHandle :: (MonadIO m, PluginMonad m, IRCMonad m) => Dynamic -> m ()
dynamicHandle (DynamicQuery key users) = do
  res <- lookupCommand key
  replies <- replyLookup key users res
  mapM_ reply replies
  return ()
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

type Stats = Map String Int

withStats :: (PluginMonad m, MonadIO m) => (StateT Stats m a) -> m (Either String a)
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

data LookupResult = Empty | Exact | Guess String

replyLookup :: (MonadIO m, PluginMonad m) => String -> [String] -> LookupResult -> m [String]
replyLookup _ _ Empty = return []
replyLookup cmd arg Exact = do
  val <- unsafeQueryCommand cmd
  return $ [ case arg of

               []      -> ""
               targets -> intercalate " " targets ++ ": "
             ++ val
           ]
replyLookup _ arg (Guess cmd) = do
  val <- unsafeQueryCommand cmd
  nick <- getUser
  return $ [ nick ++ ": Did you mean " ++ cmd ++ "?"
           , case arg of
               []      -> ""
               targets -> intercalate " " targets ++ ": "
             ++ val
           ]

lookupCommand :: (MonadIO m, PluginMonad m) => String -> m LookupResult
lookupCommand str = withStats (gets Map.keys) >>= \case
  Left _ -> return Empty
  Right cmds -> return $ case result of
    Nothing -> Empty
    Just (0, _) -> Exact
    Just (dist, word') ->
      if fromIntegral dist / fromIntegral (length word') < (0.34 :: Float) then Guess word' else Empty
    where
      assos = map (\k -> (levenshteinDistance defaultEditCosts str k, k)) cmds
      result = if null assos then Nothing else Just $ minimumBy (comparing fst) assos

{-listCommands :: (MonadIO m, PluginMonad m) => Maybe Int -> m String
listCommands mpage = do
  res <- withStats $ do
    stats <- get
    let sorted = fmap fst . sortBy (flip $ comparing snd) . Map.assocs $ stats
        pages = paging sorted getPage ircLimit
    return $ if 0 <= page && page < length pages then pages !! page else "Invalid page index, the last page is number " ++ show (length pages - 1)
  return $ either id id res
  where
    special = Map.keys fixed
    getPage 0 items = "Special commands: " ++ unwords special
      ++ " - Commands sorted by use count, page 0 (use ,<n> to view page <n>): " ++ unwords items
    getPage n items = "Page " ++ show n ++ ": " ++ unwords items
    page = fromMaybe 0 mpage
-}

unsafeQueryCommand :: (MonadIO m, PluginMonad m) => String -> m String
unsafeQueryCommand cmd = do
  cmdDir <- getCommandDir
  _ <- withStats $ modify $ Map.insertWith (+) cmd 1
  liftIO (SIO.readFile $ cmdDir </> cmd)

setCommand :: (MonadIO m, PluginMonad m) => String -> String -> m String
setCommand cmd val = if not (validCmd cmd) then return "Invalid command key" else do
  file <- (</> cmd) <$> getCommandDir
  exists <- liftIO $ doesFileExist file

  oldValue <- if exists then Just <$> liftIO (SIO.readFile file) else return Nothing
  getCommandDir >>= liftIO . createDirectoryIfMissing True
  liftIO $ writeFile file val

  _ <- withStats $ modify $ Map.insertWith const cmd 0
  return $ case oldValue of
    Just old -> cmd ++ " redefined, was defined as " ++ old
    Nothing  -> cmd ++ " defined"

validCmd :: String -> Bool
validCmd str = str == takeFileName str && isValid str

delCommand :: (MonadIO m, PluginMonad m) => String -> m String
delCommand cmd = do
  file <- (</> cmd) <$> getCommandDir
  exists <- liftIO $ doesFileExist file
  if exists then do
    val <- liftIO $ SIO.readFile file
    liftIO $ removeFile file
    _ <- withStats $ modify $ Map.delete cmd
    return $ "Undefined " ++ cmd ++ ", was defined as: " ++ val
  else return $ cmd ++ " is already undefined"
