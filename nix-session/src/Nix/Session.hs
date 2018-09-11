{-# LANGUAGE NamedFieldPuns #-}

module Nix.Session where

import           Data.Fix
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Sequence
import qualified Data.Sequence                as Seq
import           Data.Set                     (Set, (\\))
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet

import           Data.List.NonEmpty           (NonEmpty (..))
import           Text.PrettyPrint.ANSI.Leijen (SimpleDoc (..), displayS,
                                               renderCompact)

import           Nix.Expr
import           Nix.Parser
import           Nix.Pretty
import           Nix.TH

data Definition = Definition
  { expr :: Text
  , env  :: VarEnv -- Can we make this Set Int?
  , uses :: Int
  } deriving Show

type VarEnv = Map VarName Int

type NixEnv = (Int, IntMap Definition, VarEnv)

start :: NixEnv
start = (0, IntMap.empty, Map.empty)

insert :: VarName -> NExpr -> NixEnv -> NixEnv
insert var expr (n, defs, env) = (n + 1, newDefs'', newEnv)
  where
    free = freeVars expr
    deps = Map.restrictKeys env free
    def = Definition
      { expr = Text.pack $ printExpr expr
      , env = deps
      , uses = 1
      }
    increaseUse d@Definition { uses } = d { uses = uses + 1 }
    -- Increase all dependencies uses by one

    newDefs = foldl (\s i -> IntMap.adjust increaseUse i s) defs (Map.elems deps)
    -- If we have overwritten a variable in the env, decrease the old definitions use by one
    -- TODO: Remove unused definitions with propagation
    newDefs' = maybe id decrease (Map.lookup var env) newDefs


    newDefs'' = IntMap.insert n def newDefs'
    newEnv = Map.insert var n env


decrease :: Int -> IntMap Definition -> IntMap Definition
decrease key map = case IntMap.lookup key map of
  Nothing -> map
  Just Definition { uses = 1, env } -> IntMap.delete key res
    where
      res = foldl (\m i -> decrease i m) map (Map.elems env)
  Just def@Definition { uses } -> IntMap.insert key def { uses = uses - 1 } map

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
toSymAssigns _ (Inherit Nothing _ _) = Left "Inherits need a scope"
toSymAssigns _ (Inherit (Just scope) keys pos) = Right $ map inherit keys
  where inherit (StaticKey varname) = (varname, Fix (NSelect scope (StaticKey varname :| []) Nothing))
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


