{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Git ( runGit
           , gitClone
           , gitMergeFF
           , gitCommitsBetween
           , gitChangedFiles
           , gitAddBranchWorktree
           , gitGetRemoteBranches
           , gitFetch
           , gitAddRemote
           , gitCommitCount
           , gitGetCommit
           , gitLsFiles
           ) where

import           Data.Functor

import           System.Directory
import           System.Exit
import           System.Process

import           Control.Monad.Fail     hiding (fail)
import           Control.Monad.IO.Class

type Commit = String
type Remote = String
type Branch = String

data Command a = Command
  { name  :: String
  , args  :: [String]
  , parse :: forall m . MonadFail m => String -> m a
  }

gitLsFiles :: Command [FilePath]
gitLsFiles = Command "file listing" ["ls-files"] (return . lines)

gitGetCommit :: Command Commit
gitGetCommit = Command "commit getter" ["rev-parse", "HEAD"] return

gitCommitCount :: FilePath -> Command Int
gitCommitCount file = Command ("commit count for file " ++ file)
  ["log", "--pretty=format:", "--", file]
  (return . length . lines)

gitAddRemote :: String -> Remote -> Command ()
gitAddRemote remote name = Command "add remote" ["remote", "add", name, remote] (return . const ())

gitFetch :: Remote -> Command ()
gitFetch remote = Command "fetch remote" ["fetch", remote] (return . const ())

gitGetRemoteBranches :: Remote -> Command [String]
gitGetRemoteBranches remote = Command "get remote branches"
  ["branch", "--list", "-r", remote ++ "/*"]
  (return . map (tail . dropWhile (/='/')) . lines)

gitAddBranchWorktree :: Branch -> Remote -> FilePath -> Command ()
gitAddBranchWorktree branch remote target = Command ("set up worktree " ++ target ++ " for branch " ++ branch ++ " and remote " ++ remote)
  ["worktree", "add", "--track", "-b", branch, target, remote ++ "/" ++ branch]
  (return . const ())

gitMergeFF :: Command ()
gitMergeFF = Command "merge remote tracking branch fast forward" ["merge", "--ff-only"] (return . const ())

gitCommitsBetween :: Commit -> Commit -> Command [Commit]
gitCommitsBetween from to = Command "commits inbetween 2 commits" ["rev-list", from ++ ".." ++ to] (return . lines)

gitChangedFiles :: Commit -> Command [FilePath]
gitChangedFiles commit = Command "changed files in a commit" ["diff-tree", "--no-commit-id", "--name-only", "-r", commit] (return . lines)


gitClone :: (MonadFail m, MonadIO m) => String -> FilePath -> m ()
gitClone repo target = git ["clone", repo, target] $> ()

git :: (MonadFail m, MonadIO m) => [String] -> m String
git args = liftIO $ do
  mgit <- findExecutable "git"
  case mgit of
    Nothing -> fail "No git executable was found on PATH"
    Just exe -> do
      (code, stdout, stderr) <- readProcessWithExitCode exe args ""
      case code of
        ExitFailure failcode -> fail $ "Git call with arguments " ++ show args ++ " failed with exit code " ++ show failcode ++ " and error " ++ stderr
        ExitSuccess -> return stdout

runGit :: (MonadFail m, MonadIO m) => FilePath -> Command a -> m a
runGit gitdir Command { name, args, parse } = do
  output <- git ("-C":gitdir:args)
  case parse output of
    Nothing     -> fail $ "Git " ++ name ++ ": " ++ "Couldn't parse result"
    Just result -> return result

