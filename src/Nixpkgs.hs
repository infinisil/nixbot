{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Nixpkgs ( MonadNixpkgs
               , updateNixpkgs
               , nixpkgsState
               , nixpkgsPath
               , masterPath
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
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State

import           System.Directory
import           System.FilePath
import qualified System.IO.Strict              as S
import           System.Process

import           Control.Concurrent.Async.Pool

import qualified Data.Text.IO                  as TextIO

import           Data.Time
import           Git

type FileIndex = [([Text], Int)]

data NixpkgsState = NixpkgsState
  { master    :: Commit
  , fileIndex :: FileIndex
  , channels  :: Map String (Commit, Maybe FilePath)
  } deriving Show

class Monad m => MonadNixpkgs m where
  updateNixpkgs :: m ()
  nixpkgsState :: m NixpkgsState
  nixpkgsPath :: m [String]
  masterPath :: m FilePath

getNixpkgsState :: (MonadReader FilePath m, MonadIO m, MonadError String m) => m NixpkgsState
getNixpkgsState = do
  dir <- ask
  masterCommit <- gitNixpkgs gitGetCommit
  indexExists <- liftIO $ doesFileExist (dir </> "index")
  index <- if indexExists then
    liftIO $ read <$> S.readFile (dir </> "index")
    else do
      index <- generateIndex (dir </> "nixpkgs")
      --liftIO $ writeFile (dir </> "index") (show index)
      return index
  channels <- liftIO $ listDirectory (dir </> "channels")
  commits <- forM channels (\chan -> liftA2 (,) (gitChan chan gitGetCommit)
                             (do
                                exists <- liftIO $ doesDirectoryExist (dir </> "locatedb" </> chan)
                                if exists then do
                                  liftIO $ putStrLn ("Found locate db for " ++ chan ++ " at " ++ dir </> "locatedb" </> chan)
                                  return $ Just (dir </> "locatedb" </> chan)
                                else do
                                  liftIO $ putStrLn ("Couldn't find locate db for " ++ chan ++ ", tried" ++ dir </> "locatedb" </> chan)
                                  return Nothing
                              ))
  let channelCommits = Map.fromList $ zip channels commits
  return $ NixpkgsState masterCommit index channelCommits

generateIndex :: (MonadIO m, MonadError String m) => FilePath -> m FileIndex
generateIndex root = liftIO $ withTaskGroup 8 $ \taskgroup -> do
  Right gr <- runExceptT $ runGit root gitLsFiles
  let files = map (Text.split (=='/') . Text.pack) gr
  let folders = filter (not . null ) . nub . map init $ files
  let everything = folders ++ files
  counts <- mapTasks taskgroup $ map count everything
  return . sortBy (flip (comparing snd)) . zip (map reverse everything) $ counts
  undefined
  where
    count path = do
      Right r <- runExceptT $ runGit root $ gitCommitCount (Text.unpack $ Text.intercalate "/" path)
      return r

gitNixpkgs :: (MonadReader FilePath m, MonadIO m, MonadError String m) => Command a -> m a
gitNixpkgs command = do
  dir <- ask
  runGit (dir </> "nixpkgs") command

gitChan :: (MonadReader FilePath m, MonadIO m, MonadError String m) => String -> Command a -> m a
gitChan chan command = do
  dir <- ask
  runGit (dir </> "channels" </> chan) command

initNixpkgs :: (MonadReader FilePath m, MonadIO m, MonadError String m) => m NixpkgsState
initNixpkgs = do
  dir <- ask
  exists <- liftIO $ doesPathExist dir
  unless exists $ do
    gitClone "https://github.com/NixOS/nixpkgs" (dir </> "nixpkgs")
    gitNixpkgs $ gitAddRemote "https://github.com/NixOS/nixpkgs-channels" "channels"
    gitNixpkgs $ gitFetch "channels"

    channels <- gitNixpkgs $ gitGetRemoteBranches "channels"
    forM_ channels $ \chan -> do
      gitNixpkgs $ gitAddBranchWorktree chan "channels" (dir </> "channels" </> chan)
      liftIO $ createDirectoryIfMissing True (dir </> "locatedb")
      tryNixIndex (dir </> "locatedb" </> chan) (dir </> "channels" </> chan)

  getNixpkgsState

tryNixIndex :: MonadIO m => FilePath -> FilePath -> m ()
tryNixIndex database nixpkgs = do
  liftIO $ (readProcess "/run/current-system/sw/bin/nix-index" ["-d", database, "-f", nixpkgs] "" *> return ()) `catch` \e ->
    print (e :: SomeException)
  return ()

newtype NixpkgsT m a = NixpkgsT (StateT NixpkgsState (ReaderT FilePath m) a) deriving (Functor, Applicative, Monad, MonadIO)

runNixpkgsT :: (MonadIO m) => NixpkgsT m a -> m a
runNixpkgsT (NixpkgsT r) = do
  dir <- liftIO $ getXdgDirectory XdgCache "nixbot/nixpkgs"
  flip runReaderT dir $ do
    Right init <- runExceptT initNixpkgs
    evalStateT r init

instance MonadTrans NixpkgsT where
  lift = NixpkgsT . lift . lift

instance MonadNixpkgs m => MonadNixpkgs (StateT s m) where
  updateNixpkgs = lift updateNixpkgs
  nixpkgsState = lift nixpkgsState
  nixpkgsPath = lift nixpkgsPath
  masterPath = lift masterPath

instance MonadNixpkgs m => MonadNixpkgs (LoggingT m) where
  updateNixpkgs = lift updateNixpkgs
  nixpkgsState = lift nixpkgsState
  nixpkgsPath = lift nixpkgsPath
  masterPath = lift masterPath

instance MonadNixpkgs m => MonadNixpkgs (ReaderT r m) where
  updateNixpkgs = lift updateNixpkgs
  nixpkgsState = lift nixpkgsState
  nixpkgsPath = lift nixpkgsPath
  masterPath = lift masterPath

instance (MonadIO m) => MonadNixpkgs (NixpkgsT m) where
  updateNixpkgs = NixpkgsT $ do
    x <- runExceptT $ do
      oldState <- get
      dir <- ask
      gitNixpkgs $ gitFetch "origin"
      gitNixpkgs gitMergeFF
      masterCommit <- gitNixpkgs gitGetCommit

      gitNixpkgs $ gitFetch "channels"
      chans <- gitNixpkgs $ gitGetRemoteBranches "channels"
      channelCommits <- forM chans $ \chan -> do
        exists <- liftIO $ doesDirectoryExist (dir </> "channels" </> chan)
        if exists
          then gitChan chan gitMergeFF
          else gitNixpkgs $ gitAddBranchWorktree chan "channels" (dir </> "channels" </> chan)

        let oldCommit = fst <$> Map.lookup chan (channels oldState)
        newCommit <- gitChan chan gitGetCommit
        when (oldCommit /= Just newCommit) $ tryNixIndex (dir </> "locatedb" </> chan) (dir </> "channels" </> chan)

        dbexists <- liftIO $ doesDirectoryExist (dir </> "locatedb" </> chan)
        return (chan, (newCommit, if dbexists then Just (dir </> "locatedb" </> chan) else Nothing))

      newCommits <- gitNixpkgs $ gitCommitsBetween (master oldState) masterCommit
      let oldIndex = Map.fromList $ fileIndex oldState
      increases <- fmap concat $ forM newCommits $ \commit ->
        map (reverse . Text.split (=='/') . Text.pack) <$> gitNixpkgs (gitChangedFiles commit)
      let newIndex = sortBy (flip (comparing snd)) . Map.toList $ foldr (\key -> Map.insertWith (+) key 1) oldIndex increases
      --liftIO $ writeFile (dir </> "index") (show newIndex)
      put $ NixpkgsState masterCommit newIndex (Map.fromList channelCommits)
      return ()
    case x of
      Left error -> liftIO $ putStrLn error
      Right xx   -> return xx

  nixpkgsState = NixpkgsT get
  nixpkgsPath = NixpkgsT $ do
    dir <- ask
    return ["nixpkgs=" ++ dir </> "nixpkgs", dir </> "channels"]

  masterPath = NixpkgsT $ do
    dir <- ask
    return (dir </> "nixpkgs")
