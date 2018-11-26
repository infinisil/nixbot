{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix.Session.Types where

import           Control.Lens
import           Data.Aeson
import           Data.SafeCopy

import           Data.IntMap   (IntMap)
import           Data.Map      (Map)
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

-- So we don't need to depend on hnix
type VarName = Text

data SessionConfig = SessionConfig
  { _selfName  :: Text
  , _metaName  :: Text
  , _fixedDefs :: Map Text Text
  } deriving (Show, Generic)

data GlobalConfig = GlobalConfig
  { _primarySessionFile    :: FilePath
  , _secondarySessionFiles :: Map Text FilePath
  , _sessionDefaults       :: SessionConfig
  , _nixPath               :: Maybe [String]
  , _nixOptions            :: Map String String
  } deriving (Show, Generic)



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
  } deriving (Show, Read)

data Env = Env
  { _nixInstantiate    :: FilePath -- ^ Path to nix-instantiate binary
  , _primarySession    :: Session -- ^ primary session, have read-write access to this
  , _secondarySessions :: Map Text Session -- ^ Secondary sessions, read-only access to these
  , _globalConfig      :: GlobalConfig -- ^ The global nix-session configuration
  } deriving (Show)

-- | All data associated with a specific session, ultimately to be serialized to files for session persistence.
data Session = Session
  { _sessionFile   :: FilePath -- ^ The file this session is (to be) stored in
  , _sessionState  :: SessionState -- ^ The state of the session, will change with new definitions issued
  , _sessionConfig :: SessionConfig -- ^ The session config, changes infrequently and needs special handling to be changeable at all
  } deriving (Show)


lensOptions = defaultOptions { fieldLabelModifier = tail }

instance FromJSON SessionConfig where
  parseJSON = genericParseJSON lensOptions

instance FromJSON GlobalConfig where
  parseJSON = genericParseJSON lensOptions


makeLenses ''Definition
makeLenses ''SessionState
makeLenses ''Env
makeLenses ''Session
makeLenses ''SessionConfig
makeLenses ''GlobalConfig

deriveSafeCopy 1 'base ''Definition
deriveSafeCopy 1 'base ''SessionState
deriveSafeCopy 1 'base ''Session
deriveSafeCopy 1 'base ''SessionConfig

