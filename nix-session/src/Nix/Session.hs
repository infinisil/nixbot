{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Nix.Session where

import           Control.Lens                            hiding (uses)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson                              (decode')
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy.Char8              as BSL
import           Data.Fix
import           Data.Foldable                           (traverse_)
import           Data.IntMap                             (IntMap)
import qualified Data.IntMap                             as IntMap
import           Data.IntSet                             (IntSet)
import qualified Data.IntSet                             as IntSet
import           Data.List                               (intercalate)
import           Data.List.NonEmpty                      (NonEmpty (..))
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Data.Maybe
import           Data.SafeCopy
import           Data.Sequence
import qualified Data.Sequence                           as Seq
import           Data.Serialize                          (runGet, runPut)
import           Data.Set                                (Set, (\\))
import qualified Data.Set                                as Set
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Data.Text.Lens
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Nix.Expr
import           Nix.Parser
import           Nix.Pretty
import           Nix.Session.Config
import           Nix.Session.Input
import           Nix.Session.Types                       hiding (VarName)
import           Nix.TH
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process                          hiding (Inherit)

initEnv :: GlobalConfig -> IO (Either String Env)
initEnv config = do
  nixExe <- maybe (Left "Couldn't find nix-instantiate executable") Right <$> findExecutable "nix-instantiate"
  primary <- initSession (config^.primarySessionFile)
  secondaries <- sequence <$> mapM initSession (config^.secondarySessionFiles)
  return $ Env <$> nixExe <*> primary <*> secondaries <*> pure config

saveEnv :: (MonadIO m, MonadState Env m) => m ()
saveEnv = do
  primState <- use primarySession
  file <- use $ globalConfig . primarySessionFile
  let bs = runPut (safePut primState)
  liftIO $ createDirectoryIfMissing True (dropFileName file)
  liftIO $ BS.writeFile file bs

initSession :: FilePath -> IO (Either String Session)
initSession path = do
  exists <- doesFileExist path
  if exists then runGet safeGet <$> BS.readFile path
  else return $ Right newSession
  where newSession = Session { _sessionFile = path
                             , _sessionState = SessionState 0 IntMap.empty Map.empty Set.empty
                             }

increase :: MonadState SessionState m => Maybe Int -> m ()
increase Nothing    = return ()
increase (Just key) = definitions . ix key . numUses += 1

--
decrease :: MonadState SessionState m => Maybe Int -> m ()
decrease Nothing = return ()
decrease (Just key) = use (definitions . at key) >>= \case
  Nothing -> return ()
  Just Definition { _numUses = 0, _depends } -> do
    definitions . at key .= Nothing
    traverse_ decrease _depends
  Just Definition { _numUses } -> definitions . ix key . numUses .= _numUses - 1

trans :: MonadState SessionState m => VarName -> NExpr -> m ()
trans var expr = do
  -- The dependencies of the expression are all definitions in scope limited to the free variables of the expression
  -- Because only the free variables might have an influence on it.
  scope <- use roots
  let deps = Map.fromSet (`Map.lookup` scope) (freeVars expr)
  traverse_ increase deps
  use (roots . at var) >>= decrease

  let text = Text.pack $ printExpr expr
  let e = Definition var text deps 0

  defId <- use defCount
  defCount += 1
  definitions %= IntMap.insert defId e
  roots %= Map.insert var defId

genSuper :: Set VarName -> VarName
genSuper frees = head . Prelude.filter (not . (`Set.member` frees)) $
  ("super"<>) . Text.pack . show <$> [0..]

encodeEval :: Session -> Text -> Text
encodeEval session expression = final
  where
    -- Wraps an expression e in
    -- ( e ).extends (self: super: { <var> = <val>; }) for a definition
    extend e def = "(" <> e <> ")\n\t\t" <>
      ".extend (self: " <> super <> ": with " <> super <> "; { " <>
      def^.varname <> " = " <> def^.expr <> "; })"
      where super = genSuper (Map.keysSet $ def^.depends)

    -- The initial expression for above wrapping
    start = "\n\t\t(import <nixpkgs/lib>).makeExtensible (self: {})"

    -- Combine all definitions with above two functions
    chain = foldl extend start $ session^.sessionState.definitions

    final = "let self = " <>
      chain <> "; in with self;\n" <> expression


attrNames :: (MonadIO m, MonadReader Env m) => NExpr -> m (Either String [Text])
attrNames e = do
  session <- view primarySession
  let encoded = encodeEval session ("builtins.attrNames " <> Text.pack (printExpr e))

  nixInstantiateExe <- liftIO $ fromJust <$> findExecutable "nix-instantiate"
  let args = ["--eval", "--strict", "--json", "-E", Text.unpack encoded ]
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode nixInstantiateExe args ""
  case exitCode of
    ExitSuccess   -> case decode' $ BSL.pack stdout of
      Nothing   -> return $ Left "Couldn't parse json output"
      Just list -> return $ Right list
    ExitFailure _ -> return $ Left stderr

-- | Evaluate an expression in
eval :: (MonadIO m, MonadReader Env m) => NExpr -> m String
eval expr = do
  session <- view primarySession
  let encoded = encodeEval session (Text.pack (printExpr expr))
  liftIO $ putStrLn (Text.unpack encoded)

  nixInstantiateExe <- liftIO $ fromJust <$> findExecutable "nix-instantiate"
  let args = ["--eval", "-E", Text.unpack encoded ]
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode nixInstantiateExe args ""
  case exitCode of
    ExitSuccess   -> return stdout
    ExitFailure _ -> return stderr

startState = SessionState 0 IntMap.empty Map.empty

processText :: (MonadIO m, MonadState Env m) => Text -> m String
processText line = do
  parsed <- runExceptT $ parseInput line
  result <- case parsed of
    Left err -> return $ "error: " ++ show err ++ "\n"
    Right (Nix (Assignments bindings)) -> do
      definedVars <- Map.keysSet <$> use (primarySession.sessionState.roots)
      case allAssigns definedVars . fmap (fmap stripAnnotation) $ bindings of
        Left err      -> return err
        Right assigns -> do
          fixd <- use $ primarySession.sessionState.fixed
          let fixedDefs = fixd `Set.intersection` Set.fromList (map fst assigns)
          case Set.size fixedDefs of
            0 -> do
              forM_ assigns $ \(var, expr) -> do
                ss <- use (primarySession.sessionState)
                let news = execState (trans var expr) ss
                primarySession.sessionState .= news
              return "Did assign\n"
            1 -> return $ "Only admins can assign to fixed variable " ++ Text.unpack (Set.elemAt 0 fixedDefs) ++ "\n"
            _ -> return $ "Only admins can assign to fixed variables " ++ intercalate ", " (map Text.unpack (Set.toList fixedDefs)) ++ "\n"
    Right (Nix (Evaluation expr)) ->
      get >>= runReaderT (eval (stripAnnotation expr))
    Right (Command (ViewDefinition var)) -> do
      mexprnum <- use $ primarySession.sessionState.roots.at var
      case mexprnum of
        Nothing -> return "No such variable\n"
        Just exprnum -> do
          mexpr <- use $ primarySession.sessionState.definitions.ix exprnum.expr
          return . Text.unpack $ var <> " = " <> mexpr <> "\n"
    Right (Command (FixDefinition var)) -> do
      scope <- use $ primarySession.sessionState.roots
      fixd <- use $ primarySession.sessionState.fixed
      case (var `Map.member` scope, var `Set.member` fixd) of
        (False, _) -> return "No such variable\n"
        (True, False) -> do
          primarySession.sessionState.fixed %= Set.insert var
          return $ "Variable " ++ Text.unpack var ++ " is now fixed\n"
        (True, True) -> return $ "Variable " ++ Text.unpack var ++ " is already fixed\n"
    Right (Command (UnfixDefinition var)) -> do
      fixd <- use $ primarySession.sessionState.fixed
      if var `Set.member` fixd then do
        primarySession.sessionState.fixed %= Set.delete var
        return $ "Variable " ++ Text.unpack var ++ " now not fixed anymore\n"
      else return $ "Variable " ++ Text.unpack var ++ " isn't fixed\n"
    Right (Command (Load e)) -> do
      -- Evaluate `builtins.attrNames e`, assign keys with `inherit (e) key1 key2 ...;`
      mnames <- get >>= runReaderT (attrNames e)
      case mnames of
        Left err    -> return err
        Right names -> do
          forM_ names $ \var -> do
            ss <- use (primarySession.sessionState)
            let x = Fix (NSelect e (StaticKey var :| []) Nothing)
            let news = execState (trans var x) ss
            primarySession.sessionState .= news
          return "Loaded expression"
    Right _ -> return "Doing nothing\n"
  s <- use (primarySession . sessionState)
  return result

doNix :: Text -> NExpr
doNix input = case parseNixText input of
  Success expr -> expr
  Failure doc  -> error (show doc)

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
printExpr expr = renderString (fun (layoutCompact (prettyNix expr)))
  where
    fun SFail                = SFail
    fun SEmpty               = SEmpty
    fun (SChar char doc)     = SChar char $ fun doc
    fun (SText len text doc) = SText len text $ fun doc
    fun (SLine len doc)      = SChar ' ' $ fun doc
    fun (SAnnPush ann doc)   = SAnnPush ann $ fun doc
    fun (SAnnPop doc)        = SAnnPop $ fun doc
