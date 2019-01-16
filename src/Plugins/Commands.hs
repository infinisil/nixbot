{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugins.Commands (commandsPlugin') where

import           Control.Applicative        (liftA2)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Aeson
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Ord                   (comparing)
import           Data.Void
import           Plugins
import           System.Directory
import           System.Exit
import           System.FilePath
import qualified System.IO.Strict           as SIO
import           System.Process
import           Text.EditDistance          (defaultEditCosts,
                                             levenshteinDistance)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)
import           Text.Read                  (readMaybe)
import           Utils

data LookupResult = Empty | Exact | Guess String

replyLookup :: (MonadIO m, PluginMonad m) => String -> Maybe String -> LookupResult -> m [String]
replyLookup _ _ Empty = return []
replyLookup cmd arg Exact = do
  val <- unsafeQueryCommand cmd
  return $ [ case arg of

               Nothing     -> ""
               Just target -> target ++ ": "
             ++ val
           ]
replyLookup _ arg (Guess cmd) = do
  val <- unsafeQueryCommand cmd
  nick <- getUser
  return $ [ nick ++ ": Did you mean " ++ cmd ++ "?"
           , case arg of
               Nothing     -> ""
               Just target -> target ++ ": "
             ++ val
           ]

lookupCommand :: (MonadIO m, PluginMonad m) => String -> m LookupResult
lookupCommand str = withStats (gets M.keys) >>= \case
  Left _ -> return Empty
  Right cmds -> return $ case result of
    Nothing -> Empty
    Just (0, _) -> Exact
    Just (dist, word) ->
      if fromIntegral dist / fromIntegral (length word) < (0.34 :: Float) then Guess word else Empty
    where
      assos = map (\k -> (levenshteinDistance defaultEditCosts str k, k)) cmds
      result = if null assos then Nothing else Just $ minimumBy (comparing fst) assos

data LocateMode = Generic
                | Bin
                | Man

argsForMode :: LocateMode -> String -> [String]
argsForMode Generic arg =
  [ case arg of
    '/':_ -> arg
    _     -> '/':arg
  ]
argsForMode Bin arg =
  [ "--at-root"
  , "/bin/" ++ arg
  ]
argsForMode Man arg =
  [ "--regex"
  , "--at-root"
  , "/share/man/man[0-9]/" ++ arg ++ ".[0-9].gz"
  ]



data NixLocateResult = NixLocateResult
  { attrPath :: [String]
  , size     :: Int
  , fileType :: Char
  , path     :: FilePath
  } deriving Show

type Parser = Parsec Void String

nixLocateParser :: Parser [NixLocateResult]
nixLocateParser = many line <* eof where
  line :: Parser NixLocateResult
  line = NixLocateResult
    <$> (many (noneOf ". ") `sepBy` char '.' <?> "attribute path") <* space
    <*> (toNum <$> decimal `sepBy` char ',' <?> "file size") <* char ' '
    <*> (anySingleBut ' ' <?> "file type") <* char ' '
    <*> (many (anySingleBut '\n') <?> "file path") <* newline

  toNum :: [Int] -> Int
  toNum = sum . zipWith (*) (iterate (*1000) 1) . reverse

nixLocate' :: MonadIO m => LocateMode -> Bool -> String -> m (Either String [NixLocateResult])
nixLocate' locateMode whole file = do
  locateBin <- liftIO $ fromMaybe (error "Couldn't find nix-locate executable") <$> findExecutable "nix-locate"
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode locateBin
    ("--top-level":argsForMode locateMode file ++ [ "--whole-name" | whole ]) ""
  case exitCode of
    ExitFailure code -> return $ Left $ "nix-locate: Error(" ++ show code ++ "): " ++ show stderr ++ show stdout
    ExitSuccess -> case parse nixLocateParser "nix-locate-output" stdout of
      Left err  -> do
        liftIO $ putStrLn stdout
        liftIO $ parseTest nixLocateParser stdout
        return $ Left $ "nix-locate output parsing error: " ++ show err
      Right res -> return $ Right res

nixLocate :: MonadIO m => LocateMode -> String -> m (Either String [String])
nixLocate locateMode file = do
  whole <- nixLocate' locateMode True file
  fmap selectAttrs <$> case whole of
    Left err -> return $ Left err
    Right [] -> nixLocate' locateMode False file
    Right _  -> return whole

selectAttrs :: [NixLocateResult] -> [String]
selectAttrs input = sortOn (liftA2 (,) length id) $ map (stripSuffix ".out" . intercalate "." . attrPath) input

stripSuffix :: String -> String -> String
stripSuffix suffix str = if suffix `isSuffixOf` str then
  take (length str - length suffix) str else str

ircLimit :: String -> Bool
ircLimit = (<456) . length

doNixLocate :: MonadIO m => LocateMode -> String -> m String
doNixLocate locateMode arg = do
  attrs <- nixLocate locateMode arg
  return $ case attrs of
    Left err -> err
    Right [] -> "Couldn't find in any packages"
    Right packages -> fromMaybe "Found in packages, but the package attribute is too long for an IRC message.."
      $ mostMatching packages present ircLimit
      where
        present (shown, extra) = "Found in packages: " ++ intercalate ", " shown ++
          if null extra then "" else ", and " ++ show (length extra) ++ " more"

fixed :: Map String (String -> [String] -> IO (Maybe String))
fixed = M.fromList
  [ ("tell", const $ \_ -> return Nothing)
  , ("find", const $ \_ -> return Nothing)
  , ("locate", const $ \case
    [] -> return $ Just "Use ,locate <filename> to find packages containing such a file. Powered by nix-index (local installation recommended)."
    [arg] -> Just <$> doNixLocate Generic arg
    "bin":[arg] -> Just <$> doNixLocate Bin arg
    "man":[arg] -> Just <$> doNixLocate Man arg
    [tp, _] -> return $ Just $ "Unknown locate type " ++ tp
    _ -> return $ Just ",locate only takes 1 or 2 arguments"
    )
  ]

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
    else return $ Right M.empty
  case stats of
    Left err -> return $ Left err
    Right st -> do
      (res, newStats) <- runStateT action st
      liftIO $ encodeFile statFile newStats
      return $ Right res

listCommands :: (MonadIO m, PluginMonad m) => Maybe Int -> m String
listCommands mpage = do
  res <- withStats $ do
    stats <- get
    let sorted = fmap fst . sortBy (flip $ comparing snd) . M.assocs $ stats
        pages = paging sorted getPage ircLimit
    return $ if 0 <= page && page < length pages then pages !! page else "Invalid page index, the last page is number " ++ show (length pages - 1)
  return $ either id id res
  where
    special = M.keys fixed
    getPage 0 items = "Special commands: " ++ unwords special
      ++ " - Commands sorted by use count, page 0 (use ,<n> to view page <n>): " ++ unwords items
    getPage n items = "Page " ++ show n ++ ": " ++ unwords items
    page = fromMaybe 0 mpage

unsafeQueryCommand :: (MonadIO m, PluginMonad m) => String -> m String
unsafeQueryCommand cmd = do
  cmdDir <- getCommandDir
  _ <- withStats $ modify $ M.insertWith (+) cmd 1
  liftIO (SIO.readFile $ cmdDir </> cmd)

setCommand :: (MonadIO m, PluginMonad m) => String -> String -> m String
setCommand cmd val = case M.lookup cmd fixed of
  Just _ -> return $ "Can't reassign fixed commands"
  Nothing -> if not (validCmd cmd) then return "Invalid command key" else do
    file <- (</> cmd) <$> getCommandDir
    exists <- liftIO $ doesFileExist file

    oldValue <- if exists then Just <$> liftIO (SIO.readFile file) else return Nothing
    getCommandDir >>= liftIO . createDirectoryIfMissing True
    liftIO $ writeFile file val

    _ <- withStats $ modify $ M.insertWith const cmd 0
    return $ case oldValue of
      Just old -> cmd ++ " redefined, was defined as " ++ old
      Nothing  -> cmd ++ " defined"

validCmd :: String -> Bool
validCmd str = str == takeFileName str && isValid str
  -- https://stackoverflow.com/q/28166131/6605742
  && str /= "you-cant-assign-this"

delCommand :: (MonadIO m, PluginMonad m) => String -> m String
delCommand cmd = case M.lookup cmd fixed of
  Just _ -> return $ "Can't undefine fixed commands"
  Nothing -> do
    file <- (</> cmd) <$> getCommandDir
    exists <- liftIO $ doesFileExist file
    if exists then do
      val <- liftIO $ SIO.readFile file
      liftIO $ removeFile file
      _ <- withStats $ modify $ M.delete cmd
      return $ "Undefined " ++ cmd ++ ", was defined as: " ++ val
    else return $ cmd ++ " is already undefined"

commandsPlugin' :: Plugin
commandsPlugin' = Plugin
  { pluginName = "commands"
  , pluginCatcher = \Input { inputMessage } -> case inputMessage of
      ',':command -> Consumed command
      _           -> PassedOn
  , pluginHandler = \command -> do
      case words command of
        [] -> listCommands >=> reply $ Nothing
        cmd:rest -> case readMaybe cmd :: Maybe Int of
          Just num -> listCommands >=> reply $ Just num
          Nothing -> case M.lookup cmd fixed of
            Just fun -> do
              nick <- getUser
              liftIO (fun nick rest) >>= \case
                Nothing -> return ()
                Just answer -> reply answer
            Nothing -> case rest of
              [] -> do
                res <- lookupCommand cmd
                replies <- replyLookup cmd Nothing res
                mapM_ reply replies
              "=":vals -> case length vals of
                  0 -> delCommand >=> reply $ cmd
                  _ -> setCommand cmd >=> reply $ unwords vals
              args -> do
                res <- lookupCommand cmd
                replies <- replyLookup cmd (Just (unwords args)) res
                mapM_ reply replies


      return ()
  }
