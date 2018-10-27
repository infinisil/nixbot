{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}

module Nix.Session where

import           Control.Lens                 hiding (uses)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString              as BS
import           Data.Fix
import           Data.Foldable                (traverse_)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.SafeCopy
import           Data.Sequence
import qualified Data.Sequence                as Seq
import           Data.Serialize               (runGet, runPut)
import           Data.Set                     (Set, (\\))
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Lens
import           Nix.Expr
import           Nix.Parser
import           Nix.Pretty
import           Nix.Session.Config
import           Nix.TH
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process               hiding (Inherit)
import           Text.PrettyPrint.ANSI.Leijen (SimpleDoc (..), displayS,
                                               renderCompact)

import           Nix.Session.Input

data Definition = Definition
  { _varname :: VarName -- ^ The variable name of this definition
  , _expr    :: Text -- ^ The assigned Nix expression
  , _depends :: Map VarName Int -- ^ The dependencies as a map from variable names to definition index
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
  , _secondarySessions :: Map String Session -- ^ Secondary sessions, read-only access to these
  , _globalConfig      :: GlobalConfig -- ^ The global nix-session configuration
  } deriving (Show)

-- | All data associated with a specific session, ultimately to be serialized to files for session persistence.
data Session = Session
  { _sessionFile   :: FilePath -- ^ The file this session is (to be) stored in
  , _sessionState  :: SessionState -- ^ The state of the session, will change with new definitions issued
  , _sessionConfig :: SessionConfig -- ^ The session config, changes infrequently and needs special handling to be changeable at all
  } deriving (Show)

makeLenses ''Definition
makeLenses ''SessionState
makeLenses ''Env
makeLenses ''Session

initEnv :: GlobalConfig -> IO (Either String Env)
initEnv config = do
  nixExe <- maybe (Left "Couldn't find nix-instantiate executable") Right <$> findExecutable "nix-instantiate"
  primary <- initSession (config^.sessionDefaults) (config^.primarySessionFile)
  secondaries <- sequence <$> mapM (initSession (config^.sessionDefaults)) (config^.secondarySessionFiles)
  return $ Env <$> nixExe <*> primary <*> secondaries <*> pure config

saveEnv :: (MonadIO m, MonadState Env m) => m ()
saveEnv = do
  primState <- use $ primarySession
  file <- use $ globalConfig . primarySessionFile
  liftIO $ putStrLn $ "Saving to " ++ file
  let bs = runPut (safePut primState)
  liftIO $ createDirectoryIfMissing True (dropFileName file)
  liftIO $ BS.writeFile file bs

initSession :: SessionConfig -> FilePath -> IO (Either String Session)
initSession defaultConfig path = do
  exists <- doesFileExist path
  if exists then runGet safeGet <$> BS.readFile path
  else return $ Right newSession
  where newSession = Session { _sessionFile = path
                             , _sessionState = SessionState 0 IntMap.empty Map.empty
                             , _sessionConfig = defaultConfig
                             }

deriveSafeCopy 1 'base ''Definition
deriveSafeCopy 1 'base ''SessionState
deriveSafeCopy 1 'base ''Session


increase :: MonadState SessionState m => Int -> m ()
increase key = definitions . ix key . numUses += 1

--
decrease :: MonadState SessionState m => Int -> m ()
decrease key = use (definitions . at key) >>= \case
  Nothing -> return ()
  Just Definition { _numUses = 0, _depends } -> do
    definitions . at key .= Nothing
    traverse_ decrease _depends
  Just Definition { _numUses } -> definitions . ix key . numUses .= _numUses - 1

trans :: MonadState SessionState m => VarName -> NExpr -> m ()
trans var expr = do
  -- The dependencies of the expression are all definitions in scope limited to the free variables of the expression
  -- Because only the free variables might have an influence on it.
  deps <- Map.restrictKeys <$> use roots <*> pure (freeVars expr)
  traverse_ increase deps
  use (roots . at var) >>= maybe (return ()) decrease

  let text = Text.pack $ printExpr expr
  let e = Definition var text deps 0

  defId <- use defCount
  defCount += 1
  definitions %= IntMap.insert defId e
  roots %= Map.insert var defId

eval :: (MonadIO m, MonadState Env m) => NExpr -> m String
eval expr = do
  defs <- use (primarySession.sessionState.definitions)
  self <- use $ globalConfig . sessionDefaults . selfName . packed
  let chain = foldl (\acc Definition { _varname, _expr } -> "(" <> acc <> ")\n\t\t.extend (" <> self <> ": super: with super; { " <> _varname <> " = " <> _expr <> "; })") ("\n\t\t(import <nixpkgs/lib>).makeExtensible (" <> self <> ": {})") defs

  fixed <- use $ globalConfig . sessionDefaults . fixedDefs

  let fixedStr = Text.concat $ map (\(n, v) -> n <> " = " <> v <> ";\n\t") (Map.assocs fixed)

  let final = Text.unpack $ "let\n\t" <> fixedStr <> self <> " = " <> chain <> "; in with " <> self <> ";\n" <> Text.pack (printExpr expr)

  -- Watch out for:
  -- super not allowed in free vars

  --liftIO $ putStrLn final
  -- TODO: Make a read env to save this
  nixInstantiateExe <- liftIO $ fromJust <$> findExecutable "nix-instantiate"
  let args = ["--eval", "-E", final ]
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode nixInstantiateExe args ""
  case exitCode of
    ExitSuccess   -> return stdout
    ExitFailure _ -> return stderr

startState = SessionState 0 IntMap.empty Map.empty



processText :: (MonadIO m, MonadState Env m) => Text -> m String
processText line = do
  parsed <- runExceptT $ parseInput line
  result <- case parsed of
    Left err -> return $ "error: " ++ show err
    Right (Nix (Assignments bindings)) -> do
      definedVars <- Map.keysSet <$> use (primarySession.sessionState.roots)
      case allAssigns definedVars . fmap (fmap stripAnnotation) $ bindings of
        Left err      -> return err
        Right assigns -> do
          forM_ assigns $ \(var, expr) -> do
            ss <- use (primarySession.sessionState)
            let news = execState (trans var expr) ss
            primarySession.sessionState .= news
          return "Did assign\n"
    Right (Nix (Evaluation expr)) -> do
      eval $ stripAnnotation expr
    Right _ -> return "Doing nothing\n"
  s <- use (primarySession . sessionState)
  --liftIO $ print s
  return result

doNix :: Text -> NExpr
doNix input = case parseNixText input of
  Success expr -> expr
  Failure doc  -> error (show doc)
--s0 = return Map.empty
--s1 = insert <*> undefined
--
--data Evaluation = Evaluation
--  { evalExpr :: Text
--  , evalDeps :: [Assignment]
--  }

allAssigns :: Set Text -> [Binding NExpr] -> Either String [(Text, NExpr)]
allAssigns set bindings = concat <$> mapM (toSymAssigns set) bindings

toSymAssigns :: Set Text -> Binding NExpr -> Either String [(Text, NExpr)]
toSymAssigns _ (Inherit mscope keys pos) = Right $ map inherit keys
  where
    inherit (StaticKey varname) = (varname, Fix expr)
      where expr = case mscope of
              Nothing    -> NSym varname
              Just scope -> NSelect scope (StaticKey varname :| []) Nothing
    inherit (DynamicKey _) = error "Shouldn't get here, inherits from dynamic keys shouldn't be possible"
toSymAssigns _ (NamedVar (DynamicKey _ :| _) _ _) = Left "Assignments to dynamic keys are disallowed"
toSymAssigns _ (NamedVar (StaticKey varname :| []) expr _) = return [(varname, expr)]
toSymAssigns vars (NamedVar (StaticKey varname :| path@(p:ps)) expr pos)
  | Set.member varname vars = Right [(varname, mkOper2 NUpdate (mkSym varname) (thing (mkSym varname) origPath ass))]
  | otherwise = return [(varname, set)]
    where
      binding = NamedVar (p :| ps) expr pos
      set = mkNonRecSet [binding]

      origPath = init path
      ass = mkNonRecSet [NamedVar (last path :| []) expr pos]

      thing :: NExpr -> [NKeyName NExpr] -> NExpr -> NExpr
      thing orig [] ass = ass
      thing orig (p:ps) ass = mkNonRecSet . (:[]) $
        NamedVar (p :| []) (mkIf ifCond ifThen ifElse) pos
        where
          ifCond = Fix (NHasAttr orig (p :| []))
          selected = Fix (NSelect orig (p :| []) Nothing)
          ifThen = mkOper2 NUpdate selected (thing selected ps ass)
          ifElse = case ps of
            []       -> ass
            (pp:pps) -> mkNonRecSet . (:[]) $ NamedVar (pp :| pps) ass pos

printExpr :: NExpr -> String
printExpr expr = displayS (fun (renderCompact (prettyNix expr))) ""
  where
    fun SFail                = SFail
    fun SEmpty               = SEmpty
    fun (SChar char doc)     = SChar char $ fun doc
    fun (SText len text doc) = SText len text $ fun doc
    fun (SLine len doc)      = SChar ' ' $ fun doc
    fun (SSGR sgr doc)       = SSGR sgr $ fun doc


      --thing orig [] expr = expr
      --thing orig (p:ps) expr = mkOper2 NUpdate orig set'
      --  where
      --    set' = mkNonRecSet [bind]
      --    bind = NamedVar (p :| []) if' undefined
      --    if' = mkIf (Fix (NHasAttr orig (p :| []))) (thing (Fix (NSelect orig (p :| []) Nothing)) ps expr) undefined


--parse :: Text -> Result Input
--parse input = if isAssignmentParser `canParse` input
--  then Assignments <$> parseAssign input
--  else Evaluation <$> parseEval input
--  where isAssignmentParser = whiteSpace *> nixSelector *> symbol "="



--instance Show Input where
--  show (Evaluation expr) = displayS (renderCompact (prettyNix (stripAnnotation expr))) ""
--  show (Assignments bindings) = intercalate ", " (map showBinding bindings)
--    where
--      showBinding :: Binding NExprLoc -> String
--      showBinding (NamedVar path val _) = pp ++ " = " ++ w
--        where
--          x = stripAnnotation val
--          y = prettyNix x
--          w = displayS (renderCompact y) ""
--          p = prettySelector (fmap (fmap (simpleExpr . prettyNix . stripAnnotation)) path)
--          pp = displayS (renderCompact p) ""
--      showBinding (Inherit scope keys _) = "inherit " ++ pScope scope ++ k
--        where
--          pScope Nothing = ""
--          pScope (Just n) = "(" ++ displayS (renderCompact (prettyNix (stripAnnotation n))) "" ++ ") "
--          k = intercalate " " (map prettyKey keys)

    --          prettyKey (StaticKey name) = unpack name


