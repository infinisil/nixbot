{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}

module Nix.Session where

import           Control.Lens                 hiding (uses)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
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
  { _nixInstantiate :: FilePath
  , _config         :: Config
  }

-- | All data associated with a specific session, ultimately to be serialized to files for session persistence.
data Session = Session
  { _sessionState  :: SessionState -- ^ The state of the session, will change with new definitions issued
  , _sessionConfig :: SessionConfig -- ^ The session config, changes infrequently and needs special handling to be changeable at all
  }

deriveSafeCopy 1 'base ''Definition
deriveSafeCopy 1 'base ''SessionState
deriveSafeCopy 1 'base ''Session

makeLenses ''Definition
makeLenses ''SessionState
makeLenses ''Env
makeLenses ''Session

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

eval :: (MonadIO m, MonadState SessionState m, MonadReader Env m) => NExpr -> m String
eval expr = do
  defs <- use definitions
  self <- view $ config . sessionDefaults . selfName . packed
  let chain = foldl (\acc Definition { _varname, _expr } -> "(" <> acc <> ")\n\t\t.extend (" <> self <> ": super: with super; { " <> _varname <> " = " <> _expr <> "; })") ("\n\t\t(import <nixpkgs/lib>).makeExtensible (" <> self <> ": {})") defs

  fixed <- view $ config . sessionDefaults . fixedDefs

  let fixedStr = Text.concat $ map (\(n, v) -> n <> " = " <> v <> ";\n\t") (Map.assocs fixed)

  let final = Text.unpack $ "let\n\t" <> fixedStr <> self <> " = " <> chain <> "; in with " <> self <> ";\n" <> Text.pack (printExpr expr)

  -- Watch out for:
  -- super not allowed in free vars

  liftIO $ putStrLn final
  -- TODO: Make a read env to save this
  nixInstantiateExe <- liftIO $ fromJust <$> findExecutable "nix-instantiate"
  let args = ["--eval", "-E", final ]
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode nixInstantiateExe args ""
  case exitCode of
    ExitSuccess   -> return stdout
    ExitFailure _ -> return stderr

startState = SessionState 0 IntMap.empty Map.empty



processText :: (MonadIO m, MonadState SessionState m, MonadReader Env m) => Text -> m String
processText line = do
  parsed <- runExceptT $ parseInput line
  result <- case parsed of
    Left err -> return $ "error: " ++ show err
    Right (Nix (Assignments bindings)) -> do
      definedVars <- Map.keysSet <$> use roots
      case allAssigns definedVars . fmap (fmap stripAnnotation) $ bindings of
        Left err      -> return err
        Right assigns -> do
          traverse_ (uncurry trans) assigns
          return "Did assign"
    Right (Nix (Evaluation expr)) -> do
      eval $ stripAnnotation expr
    Right _ -> return "Doing nothing"
  s <- get
  liftIO $ print s
  return result



--start :: SessionState
--start = (0, IntMap.empty, Map.empty)
--
--insert :: VarName -> NExpr -> SessionState -> SessionState
--insert var expr (n, defs, envv) = (n + 1, newDefs'', newEnv)
--  where
--    imDeps = IntSet.fromList . Map.elems . Map.restrictKeys envv . freeVars $ expr
--    deps = IntSet.foldl' (\acc i -> IntSet.union acc . maybe IntSet.empty depends . IntMap.lookup i $ defs) imDeps imDeps
--    def = Definition
--      { var = var
--      , expr = Text.pack $ printExpr expr
--      , free = freeVars expr
--      , depends = deps
--      , uses = 1
--      }
--    increaseUse d@Definition { uses } = d { uses = uses + 1 }
--    -- Increase all dependencies uses by one
--
--
--    newDefs = IntSet.foldl (\s i -> IntMap.adjust increaseUse i s) defs deps
--    -- If we have overwritten a variable in the env, decrease the old definitions use by one
--    -- TODO: Remove unused definitions with propagation
--    newDefs' = maybe id decrease (Map.lookup var envv) newDefs
--
--
--    newDefs'' = IntMap.insert n def newDefs'
--    newEnv = Map.insert var n envv
--
--deleteDef :: Int -> IntMap Definition -> IntMap Definition
--deleteDef key map = case IntMap.lookup key map of
--  Nothing -> map
--  Just Definition { depends } -> IntMap.delete key res
--    where
--      res = IntSet.foldl (\m i -> decrease i m) map depends
--
--decrease :: Int -> IntMap Definition -> IntMap Definition
--decrease key map = case IntMap.lookup key map of
--  Nothing -> map
--  Just Definition { uses = 1, depends } -> IntMap.delete key res
--    where
--      res = IntSet.foldl (\m i -> decrease i m) map depends
--  Just def@Definition { uses } -> IntMap.insert key def { uses = uses - 1 } map
--
--depNames :: IntMap Definition -> Definition -> Set VarName
--depNames m Definition { depends } = undefined var depends
--
--conflicts :: Definition -> Definition -> Bool
--conflicts Definition { free = lf } Definition { free = rf } = undefined
--
--eval :: SessionState -> NExpr -> ()
--eval (_, defs, env) expr = undefined
--  where
--    imDeps = Map.elems . Map.restrictKeys env . freeVars $ expr
--    trans = foldl (\acc i -> acc `IntSet.union` maybe IntSet.empty depends (IntMap.lookup i defs)) IntSet.empty imDeps
--    toRemove = IntMap.keysSet defs IntSet.\\ trans
--    new = IntSet.foldl (\d s -> deleteDef s d) defs toRemove
--
--    unused :: IntMap Definition -> IntSet
--    unused = IntMap.keysSet . IntMap.filter (\Definition { uses } -> uses == 1)
--
--    select :: IntMap Definition -> IntSet -> IntSet
--    select map set = fst $ IntSet.foldl (\(ai, as) el -> if Set.null (as `Set.intersection` maybe Set.empty free (IntMap.lookup el map)) then (IntSet.insert el ai, as `Set.union` maybe Set.empty free (IntMap.lookup el map)) else (ai, as)) (IntSet.empty, Set.empty) set
--      where



--insert :: VarName -> NExpr -> VarEnv -> VarEnv
--insert var expr env = Map.insert var def env
--  where
--    freePlusSelf = Set.insert var $ freeVars expr
--    deps = Map.restrictKeys env freePlusSelf
--    def = Definition (Text.pack $ printExpr expr) deps


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


