{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_0 (
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
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X
import GHC.Parser.Annotation as X (AnnotationComment (..), getAnnotationComments)
import GHC.Plugins as X hiding (getHscEnv, srcSpanStart, varName)
import GHC.Types.Name.Cache as X (NameCache)

import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)
import qualified GHC.Types.SrcLoc as GHC (srcSpanStart)

import Test.Tasty.AutoCollect.GHC.Shim_Common

{----- Compat / SrcSpan -----}

fromRealSrcSpan :: RealSrcSpan -> SrcSpan
fromRealSrcSpan x = RealSrcSpan x Nothing

toRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
toRealSrcSpan (RealSrcSpan x _) = Just x
toRealSrcSpan (UnhelpfulSpan _) = Nothing

srcSpanStart :: SrcSpan -> Either String RealSrcLoc
srcSpanStart ss =
  case GHC.srcSpanStart ss of
    RealSrcLoc srcLoc _ -> Right srcLoc
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
    ValD _ (FunBind _ name MG{mg_alts = L _ matches} _) ->
      Just $ FuncDef name $ map (fmap parseFuncSingleDef) matches
    _ -> Nothing
  where
    parseFuncSingleDef Match{m_pats, m_grhss = GRHSs _ bodys whereClause} =
      FuncSingleDef
        { funcDefArgs = m_pats
        , funcDefGuards = map parseFuncGuardedBody bodys
        , funcDefWhereClause = whereClause
        }
    parseFuncGuardedBody (L _ (GRHS _ guards body)) =
      FuncGuardedBody guards body

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
    ConPat _ name details ->
      PatConstructor name $
        case details of
          PrefixCon args -> PrefixCon $ map parsePat args
          RecCon fields -> RecCon $ parsePat <$> fields
          InfixCon l r -> InfixCon (parsePat l) (parsePat r)
    ViewPat{} -> PatView
    SplicePat _ splice -> PatSplice splice
    LitPat _ lit -> PatLiteral lit
    NPat _ lit _ _ -> PatOverloadedLit lit
    NPlusKPat{} -> PatNPlusK
    SigPat _ p (HsPS _ ty) -> PatTypeSig (parsePat p) ty
