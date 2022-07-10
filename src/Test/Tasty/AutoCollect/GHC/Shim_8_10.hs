{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GHC.Shim_8_10 (
  -- * Re-exports
  module X,

  -- * Compat

  -- ** SrcSpan
  fromRealSrcSpan,
  toRealSrcSpan,
  srcSpanStart,

  -- ** OccName
  mkOccNameVar,
  mkOccNameTC,

  -- ** Decl
  parseDecl,

  -- ** Pat
  parsePat,

  -- * Backports
  getAnnotationComments,
  unLoc,
  getLoc,
  mkHsApps,
  generatedSrcSpan,
) where

-- Re-exports
import ApiAnnotation as X (AnnotationComment (..))
import GHC.Hs as X
import GhcPlugins as X hiding (getHscEnv, getLoc, srcSpanStart, unLoc)
import HscMain as X (getHscEnv)
import NameCache as X (NameCache)

import ApiAnnotation (ApiAnns)
import qualified ApiAnnotation as GHC (getAnnotationComments)
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import qualified OccName as NameSpace (tcName, varName)
import qualified SrcLoc as GHC (srcSpanStart)

import Test.Tasty.AutoCollect.GHC.Shim_Common

{----- Compat / SrcSpan -----}

fromRealSrcSpan :: RealSrcSpan -> SrcSpan
fromRealSrcSpan = RealSrcSpan

toRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
toRealSrcSpan (RealSrcSpan x) = Just x
toRealSrcSpan (UnhelpfulSpan _) = Nothing

srcSpanStart :: SrcSpan -> Either String RealSrcLoc
srcSpanStart ss =
  case GHC.srcSpanStart ss of
    RealSrcLoc srcLoc -> Right srcLoc
    UnhelpfulLoc s -> Left $ unpackFS s

{----- Compat / OccName -----}

mkOccNameVar :: String -> OccName
mkOccNameVar = mkOccName NameSpace.varName

mkOccNameTC :: String -> OccName
mkOccNameTC = mkOccName NameSpace.tcName

{----- Compat / Decl -----}

parseDecl :: LHsDecl GhcPs -> Maybe ParsedDecl
parseDecl (L _ decl) =
  case decl of
    SigD _ (TypeSig _ names ty) -> Just $ FuncSig names ty
    ValD _ (FunBind _ name matchGroup _ _) ->
      Just . FuncDef name $
        case matchGroup of
          MG{mg_alts = L _ matches} -> map (fmap parseFuncSingleDef) matches
          XMatchGroup x -> noExtCon x
    _ -> Nothing
  where
    parseFuncSingleDef = \case
      Match{m_pats, m_grhss = GRHSs _ bodys whereClause} ->
        FuncSingleDef
          { funcDefArgs = m_pats
          , funcDefGuards = map (parseFuncGuardedBody . unLoc) bodys
          , funcDefWhereClause = whereClause
          }
      Match{m_grhss = XGRHSs x} -> noExtCon x
      XMatch x -> noExtCon x
    parseFuncGuardedBody = \case
      GRHS _ guards body -> FuncGuardedBody guards body
      XGRHS x -> noExtCon x

{----- Compat / Pat -----}

parsePat :: LPat GhcPs -> ParsedPat
parsePat (L _ pat) =
  case pat of
    WildPat{} -> PatWildCard
    VarPat _ name -> PatVar name
    LazyPat{} -> PatLazy
    AsPat{} -> PatAs
    ParPat _ p -> PatParens (parsePat p)
    BangPat{} -> PatBang
    ListPat _ ps -> PatList (map parsePat ps)
    TuplePat _ ps boxity -> PatTuple (map parsePat ps) boxity
    SumPat{} -> PatSum
    ConPatIn name details ->
      PatConstructor name $
        case details of
          PrefixCon args -> PrefixCon $ map parsePat args
          RecCon fields -> RecCon $ parsePat <$> fields
          InfixCon l r -> InfixCon (parsePat l) (parsePat r)
    ConPatOut{} -> onlyTC "ConPatOut"
    ViewPat{} -> PatView
    SplicePat _ splice -> PatSplice splice
    LitPat _ lit -> PatLiteral lit
    NPat _ lit _ _ -> PatOverloadedLit lit
    NPlusKPat{} -> PatNPlusK
    SigPat _ p (HsWC _ (HsIB _ ty)) -> PatTypeSig (parsePat p) ty
    CoPat{} -> onlyTC "CoPat"
    -- impossible cases that GHC 8.10 isn't smart enough to prune
    SigPat _ _ (HsWC _ (XHsImplicitBndrs x)) -> noExtCon x
    SigPat _ _ (XHsWildCardBndrs x) -> noExtCon x
    XPat x -> noExtCon x
  where
    -- https://gitlab.haskell.org/ghc/ghc/-/commit/c42754d5fdd3c2db554d9541bab22d1b3def4be7
    onlyTC label = error $ "Unexpectedly got: " ++ label

{----- Backports -----}

getAnnotationComments :: ApiAnns -> RealSrcSpan -> [RealLocated AnnotationComment]
getAnnotationComments anns = mapMaybe toRealLocated . GHC.getAnnotationComments anns . RealSrcSpan
  where
    toRealLocated = \case
      L (RealSrcSpan l) e -> Just (L l e)
      L (UnhelpfulSpan _) _ -> Nothing

unLoc :: GenLocated l e -> e
unLoc (L _ e) = e

getLoc :: GenLocated l e -> l
getLoc (L l _) = l

mkHsApps :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkHsApps = foldl' mkHsApp

generatedSrcSpan :: SrcSpan
generatedSrcSpan = UnhelpfulSpan (fsLit "<generated>")
