{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Session.Types where

import           Control.Lens
import           Data.Aeson
import           Data.SafeCopy

import           Data.IntMap   (IntMap)
import           Data.Map      (Map)
import           Data.Set      (Set)
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

-- So we don't need to depend on hnix
type VarName = Text


data GlobalConfig = GlobalConfig
  { _primarySessionFile    :: FilePath
  , _secondarySessionFiles :: Map Text FilePath
  , _nixPath               :: Maybe [String]
  , _nixOptions            :: Map String String
  } deriving (Show, Generic)

-- TODO: Remember whether a definition could be changeable by other variables, aka has self in freeVars or any of its dependencies are changeable
-- TODO: Different format on disk vs in memory: No need to store _depends on disk, can be computed on startup (also to make sure the current nix version still works with it). Essentially go through all commands again. Maybe we just need to store an operation log then! To make it faster, cache the result, should be invalidated based on all impure inputs, such as the nix version
data Definition = Definition
  { _varname :: VarName -- ^ The variable name of this definition
  , _expr    :: Text -- ^ The assigned Nix expression
  , _depends :: Map VarName (Maybe Int) -- ^ A map from all free variables to either the definition index they'd use as its dependency or Nothing when there was no such variable in scope
  , _numUses :: Int -- ^ How many dependents this definition has
  } deriving (Show, Read)

data SessionState = SessionState
  { _defCount    :: Int -- ^ How many definitions have been issued
  , _definitions :: IntMap Definition -- ^ The definitions that have been issued and are still alive, reachable by a root
  , _roots       :: Map VarName Int -- ^ The definition roots, aka the definitions currently in scope
  , _fixed       :: Set VarName -- ^ The set of definitions that can only be changed by admins
  } deriving (Show, Read)

data Env = Env
  { _nixInstantiate    :: FilePath -- ^ Path to nix-instantiate binary
  , _primarySession    :: Session -- ^ primary session, have read-write access to this
  , _secondarySessions :: Map Text Session -- ^ Secondary sessions, read-only access to these
  , _globalConfig      :: GlobalConfig -- ^ The global nix-session configuration
  } deriving (Show)

-- | All data associated with a specific session, ultimately to be serialized to files for session persistence.
data Session = Session
  { _sessionFile  :: FilePath -- ^ The file this session is (to be) stored in
  , _sessionState :: SessionState -- ^ The state of the session, will change with new definitions issued
  } deriving (Show)


lensOptions = defaultOptions { fieldLabelModifier = tail }

instance FromJSON GlobalConfig where
  parseJSON = genericParseJSON lensOptions


makeLenses ''Definition
makeLenses ''SessionState
makeLenses ''Env
makeLenses ''Session
makeLenses ''GlobalConfig

deriveSafeCopy 1 'base ''Definition
deriveSafeCopy 1 'base ''SessionState
deriveSafeCopy 1 'base ''Session

