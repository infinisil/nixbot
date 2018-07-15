{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Nixpkgs ( MonadNixpkgs
               , updateNixpkgs
               , nixpkgsState
               , nixFlags
               , NixpkgsT
               , runNixpkgsT
               , Commit(..)
               , NixpkgsState(..)
               , FileIndex(..)
               ) where

import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import           Data.List
import           Data.Ord
import           Data.Tree

import           Control.Applicative
import           Control.Exception.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State

import           System.Directory
import           System.FilePath
import qualified System.IO.Strict              as S
import           System.Process

import           Control.Concurrent.Async.Pool

import qualified Data.Text.IO                  as TextIO

import           Data.Time

data Commit = Commit
  { sha  :: String
  , date :: UTCTime
  } deriving (Show)

type FileIndex = [([Text], Int)]

data NixpkgsState = NixpkgsState
  { master    :: Commit
  , fileIndex :: FileIndex
  , channels  :: Map String (Commit, Maybe FilePath)
  } deriving Show

class MonadNixpkgs m where
  updateNixpkgs :: m ()
  nixpkgsState :: m NixpkgsState
  nixFlags :: m [String]

getCommit :: MonadIO m => FilePath -> m Commit
getCommit dir = do
  [sha, timestring] <- words <$> git ["-C", dir, "log", "-1", "--format=%H %at"]
  time <- parseTimeM False defaultTimeLocale "%s" timestring
  return $ Commit sha time

getNixpkgsState :: (MonadReader FilePath m, MonadIO m) => m NixpkgsState
getNixpkgsState = do
  dir <- ask
  masterCommit <- getCommit (dir </> "nixpkgs")
  indexExists <- liftIO $ doesFileExist (dir </> "index")
  index <- if indexExists then
    liftIO $ read <$> S.readFile (dir </> "index")
    else do
      index <- generateIndex (dir </> "nixpkgs")
      liftIO $ writeFile (dir </> "index") (show index)
      return index
  channels <- liftIO $ listDirectory (dir </> "channels")
  commits <- forM channels (\chan -> liftA2 (,) (getCommit (dir </> "channels" </> chan))
                             (do
                                exists <- liftIO $ doesDirectoryExist (dir </> "locatedb" </> chan)
                                return $ if exists then Just (dir </> "locatedb" </> chan)
                                else Nothing
                              ))
  let channelCommits = Map.fromList $ zip channels commits
  return $ NixpkgsState masterCommit index channelCommits

generateIndex :: (MonadIO m) => FilePath -> m FileIndex
generateIndex root = liftIO $ withTaskGroup 8 $ \taskgroup -> do
  files <- fmap (Text.split (=='/')) . Text.lines . Text.pack <$> git ["-C", root, "ls-files"]
  let folders = filter (not . null ) . nub . map init $ files
  let everything = folders ++ files
  counts <- mapTasks taskgroup $ map count everything
  return . sortBy (flip (comparing snd)) . zip (map reverse everything) $ counts
  where
    count path = length . lines <$> git ["-C", root, "log", "--pretty=format:", "--", Text.unpack $ Text.intercalate "/" path]

initNixpkgs :: (MonadReader FilePath m, MonadIO m) => m NixpkgsState
initNixpkgs = do
  dir <- ask
  exists <- liftIO $ doesPathExist dir
  unless exists $ do
    git ["clone", "--reference", "/home/infinisil/src/nixpkgs", "--dissociate", "https://github.com/NixOS/nixpkgs", dir </> "nixpkgs"]
    git ["-C", dir </> "nixpkgs", "remote", "add", "channels", "https://github.com/NixOS/nixpkgs-channels"]
    git ["-C", dir </> "nixpkgs", "fetch", "channels"]

    channels <- map (tail . dropWhile (/='/')) . lines <$> git ["-C", dir </> "nixpkgs", "branch", "--list", "-r", "channels/*"]
    forM_ channels $ \chan -> do
      git ["-C", dir </> "nixpkgs", "worktree", "add", "--track", "-b", chan, dir </> "channels" </> chan, "channels/" ++ chan]
      liftIO $ createDirectoryIfMissing True (dir </> "locatedb")
      tryNixIndex (dir </> "locatedb" </> chan) (dir </> "channels" </> chan)

  getNixpkgsState

tryNixIndex :: MonadIO m => FilePath -> FilePath -> m ()
tryNixIndex database nixpkgs = do
  liftIO $ (readProcess "/run/current-system/sw/bin/nix-index" ["-d", database, "-f", nixpkgs] "" *> return ()) `catch` \e ->
    print (e :: SomeException)
  return ()

git :: MonadIO m => [String] -> m String
git args = do
  liftIO $ putStrLn $ "Calling git with arguments " ++ unwords args
  liftIO $ readProcess "/run/current-system/sw/bin/git" args ""

newtype NixpkgsT m a = NixpkgsT (StateT NixpkgsState (ReaderT FilePath m) a) deriving (Functor, Applicative, Monad)

runNixpkgsT :: MonadIO m => NixpkgsT m a -> m a
runNixpkgsT (NixpkgsT r) = do
  dir <- liftIO $ getXdgDirectory XdgCache "nixbot/nixpkgs"
  flip runReaderT dir $ do
    init <- initNixpkgs
    evalStateT r init

instance MonadTrans NixpkgsT where
  lift = NixpkgsT . lift . lift

instance MonadIO m => MonadNixpkgs (NixpkgsT m) where
  updateNixpkgs = NixpkgsT $ do
    oldState <- get
    dir <- ask
    git ["-C", dir </> "nixpkgs", "fetch", "--all"]
    git ["-C", dir </> "nixpkgs", "merge", "origin/master", "--ff-only"]
    masterCommit <- getCommit (dir </> "nixpkgs")
    channels <- lift $ map (tail . dropWhile (/='/')) . lines <$> git ["-C", dir </> "nixpkgs", "branch", "--list", "-r", "channels/*"]
    channelCommits <- forM channels $ \chan -> do
      exists <- liftIO $ doesDirectoryExist (dir </> "channels" </> chan)
      if exists then git ["-C", dir </> "channels" </> chan, "merge", "channels/" ++ chan, "--ff-only"]
        else git ["-C", dir </> "nixpkgs", "worktree", "add", "--track", "-b", chan, dir </> "channels" </> chan, "channels/" ++ chan]
      tryNixIndex (dir </> "locatedb" </> chan) (dir </> "channels" </> chan)

      dbexists <- liftIO $ doesDirectoryExist (dir </> "locatedb" </> chan)
      (chan,) <$> liftA2 (,) (getCommit (dir </> "channels" </> chan)) (return $ if dbexists then Just (dir </> "locatedb" </> chan) else Nothing)

    newCommits <- lines <$> git ["-C", dir </> "nixpkgs", "rev-list", sha (master oldState) ++ ".." ++ sha masterCommit]
    let oldIndex = Map.fromList $ fileIndex oldState
    increases <- concat <$> forM newCommits (countCommit (dir </> "nixpkgs"))
    let newIndex = sortBy (flip (comparing snd)) . Map.toList $ foldr (\key -> Map.insertWith (+) key 1) oldIndex increases

    liftIO $writeFile (dir </> "index") (show newIndex)
    put $ NixpkgsState masterCommit newIndex (Map.fromList channelCommits)
    return ()
    where
      countCommit :: MonadIO m => FilePath -> String -> m [[Text]]
      countCommit gitdir sha = do
        fileStr <- git ["-C", gitdir, "diff-tree", "--no-commit-id", "--name-only", "-r", sha]
        return $ map (reverse . Text.split (=='/')) $ Text.lines (Text.pack fileStr)

  nixpkgsState = NixpkgsT get
  nixFlags = NixpkgsT $ do
    dir <- ask
    return ["-I", "nixpkgs=" ++ dir </> "nixpkgs", "-I", dir </> "channels"]
