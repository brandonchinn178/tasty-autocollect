{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_0 (
  -- * Re-exports
  module X,

  -- * Compat

  -- ** Plugin
  setKeepRawTokenStream,

  -- ** Annotations
  getExportComments,
  generatedSrcAnn,
  toSrcAnnA,

  -- ** SrcSpan
  srcSpanStart,

  -- ** OccName
  mkOccNameVar,
  mkOccNameTC,

  -- ** Decl
  parseDecl,

  -- ** Type
  parseSigWcType,
  parseType,

  -- ** Pat
  parsePat,

  -- ** Expr
  mkExplicitList,
  mkExplicitTuple,
  xAppTypeE,

  -- * Backports
  SrcAnn,
  SrcSpanAnn',
  LocatedN,
  getLocA,
  mkMatch,
  noAnn,
  hsTypeToHsSigType,
  hsTypeToHsSigWcType,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X hiding (mkHsAppType, mkHsAppTypes, mkMatch)
import GHC.Parser.Annotation as X (AnnotationComment (..))
import GHC.Plugins as X hiding (getHscEnv, srcSpanStart, varName)
import GHC.Types.Name.Cache as X (NameCache)

import qualified Data.Text as Text
import qualified GHC.Hs.Utils as GHC (mkMatch)
import GHC.Parser.Annotation (getAnnotationComments)
import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)
import qualified GHC.Types.SrcLoc as GHC (srcSpanStart)

import Test.Tasty.AutoCollect.GHC.Shim_Common
import Test.Tasty.AutoCollect.Utils.Text

{----- Compat / Plugin -----}

setKeepRawTokenStream :: Plugin -> Plugin
setKeepRawTokenStream plugin =
  plugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    }

{----- Compat / Annotations -----}

-- | Get the contents of all comments in the given hsmodExports list.
getExportComments :: HsParsedModule -> Located [LIE GhcPs] -> [RealLocated String]
getExportComments parsedModl = map fromRLAnnotationComment . getCommentsAt . getLoc
  where
    getCommentsAt = \case
      RealSrcSpan x _ -> getAnnotationComments (hpm_annotations parsedModl) x
      UnhelpfulSpan _ -> []
    fromRLAnnotationComment (L rss comment) =
      L rss $ (Text.unpack . Text.strip . unwrap) comment
    unwrap = \case
      AnnDocCommentNext s -> withoutPrefix "-- |" $ Text.pack s
      AnnDocCommentPrev s -> withoutPrefix "-- ^" $ Text.pack s
      AnnDocCommentNamed s -> withoutPrefix "-- $" $ Text.pack s
      AnnDocSection _ s -> Text.pack s
      AnnDocOptions s -> Text.pack s
      AnnLineComment s -> withoutPrefix "--" $ Text.pack s
      AnnBlockComment s -> withoutPrefix "{-" . withoutSuffix "-}" $ Text.pack s

generatedSrcAnn :: SrcSpan
generatedSrcAnn = generatedSrcSpan

toSrcAnnA :: RealSrcSpan -> SrcSpan
toSrcAnnA x = RealSrcSpan x Nothing

{----- Compat / SrcSpan -----}

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
        , funcDefWhereClause = unLoc whereClause
        }
    parseFuncGuardedBody (L _ (GRHS _ guards body)) =
      FuncGuardedBody guards body

{----- Compat / Type -----}

parseSigWcType :: LHsSigWcType GhcPs -> Maybe ParsedType
parseSigWcType (HsWC _ (HsIB _ ltype)) = parseType ltype

parseType :: LHsType GhcPs -> Maybe ParsedType
parseType (L _ ty) =
  case ty of
    HsTyVar _ flag name -> Just $ TypeVar flag name
    HsListTy _ t -> TypeList <$> parseType t
    _ -> Nothing

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
          PrefixCon args -> ConstructorPrefix [] $ map parsePat args
          RecCon fields -> ConstructorRecord $ parsePat <$> fields
          InfixCon l r -> ConstructorInfix (parsePat l) (parsePat r)
    ViewPat{} -> PatView
    SplicePat _ splice -> PatSplice splice
    LitPat _ lit -> PatLiteral lit
    NPat _ lit _ _ -> PatOverloadedLit lit
    NPlusKPat{} -> PatNPlusK
    SigPat _ p (HsPS _ ty) -> PatTypeSig (parsePat p) ty

{----- Compat / Expr -----}

mkExplicitList :: [LHsExpr GhcPs] -> HsExpr GhcPs
mkExplicitList = ExplicitList noExtField Nothing

mkExplicitTuple :: [HsTupArg GhcPs] -> Boxity -> HsExpr GhcPs
mkExplicitTuple = ExplicitTuple noAnn . map (L generatedSrcAnn)

xAppTypeE :: XAppTypeE GhcPs
xAppTypeE = noExtField

{----- Backports -----}

type SrcAnn ann = SrcSpan
type SrcSpanAnn' a = SrcSpan
type LocatedN = Located

getLocA :: Located e -> SrcSpan
getLocA = getLoc

mkMatch :: HsMatchContext GhcPs -> [LPat GhcPs] -> LHsExpr GhcPs -> HsLocalBinds GhcPs -> LMatch GhcPs (LHsExpr GhcPs)
mkMatch ctxt pats expr lbinds = GHC.mkMatch ctxt pats expr (L generatedSrcAnn lbinds)

noAnn :: NoExtField
noAnn = NoExtField

hsTypeToHsSigType :: LHsType GhcPs -> LHsSigType GhcPs
hsTypeToHsSigType = mkLHsSigType

hsTypeToHsSigWcType :: LHsType GhcPs -> LHsSigWcType GhcPs
hsTypeToHsSigWcType = mkLHsSigWcType
