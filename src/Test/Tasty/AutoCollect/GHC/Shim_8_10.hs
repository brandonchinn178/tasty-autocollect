{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GHC.Shim_8_10 (
  -- * Re-exports
  module X,

  -- * Compat

  -- ** Plugin
  setKeepRawTokenStream,
  withParsedResultModule,

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

  -- ** Expr
  mkExplicitList,
  mkExplicitTuple,
  xAppTypeE,

  -- ** Pat
  parsePat,

  -- * Backports
  SrcAnn,
  SrcSpanAnn',
  LocatedN,
  unLoc,
  getLoc,
  getLocA,
  mkHsApps,
  mkMatch,
  noAnn,
  hsTypeToHsSigType,
  hsTypeToHsSigWcType,
  thNameToGhcNameIO,
) where

-- Re-exports
import ApiAnnotation as X (AnnotationComment (..))
import GHC.Hs as X hiding (mkHsAppType, mkHsAppTypes, mkMatch)
import GhcPlugins as X hiding (getHscEnv, getLoc, showPpr, srcSpanStart, unLoc)
import HscMain as X (getHscEnv)
import NameCache as X (NameCache)

import ApiAnnotation (getAnnotationComments)
import Data.Foldable (foldl')
import Data.IORef (IORef)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified GHC.Hs.Utils as GHC (mkMatch)
import qualified Language.Haskell.TH as TH
import qualified OccName as NameSpace (tcName, varName)
import qualified SrcLoc as GHC (srcSpanStart)

import Test.Tasty.AutoCollect.GHC.Shim_Common
import Test.Tasty.AutoCollect.Utils.Text

{----- Compat / Plugin -----}

setKeepRawTokenStream :: Plugin -> Plugin
setKeepRawTokenStream plugin =
  plugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    }

withParsedResultModule :: HsParsedModule -> (HsParsedModule -> HsParsedModule) -> HsParsedModule
withParsedResultModule = flip ($)

{----- Compat / Annotations -----}

-- | Get the contents of all comments in the given hsmodExports list.
getExportComments :: HsParsedModule -> Located [LIE GhcPs] -> [RealLocated String]
getExportComments parsedModl = map fromRLAnnotationComment . getCommentsAt . getLoc
  where
    getCommentsAt = mapMaybe toRealLocated . getAnnotationComments (hpm_annotations parsedModl)
    toRealLocated = \case
      L (RealSrcSpan l) e -> Just (L l e)
      L (UnhelpfulSpan _) _ -> Nothing
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
generatedSrcAnn = UnhelpfulSpan (fsLit "<generated>")

toSrcAnnA :: RealSrcSpan -> SrcSpan
toSrcAnnA = RealSrcSpan

{----- Compat / SrcSpan -----}

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
          , funcDefWhereClause = unLoc whereClause
          }
      Match{m_grhss = XGRHSs x} -> noExtCon x
      XMatch x -> noExtCon x
    parseFuncGuardedBody = \case
      GRHS _ guards body -> FuncGuardedBody guards body
      XGRHS x -> noExtCon x

{----- Compat / Type -----}

parseSigWcType :: LHsSigWcType GhcPs -> Maybe ParsedType
parseSigWcType = \case
  HsWC _ (HsIB _ ltype) -> parseType ltype
  HsWC _ (XHsImplicitBndrs x) -> noExtCon x
  XHsWildCardBndrs x -> noExtCon x

parseType :: LHsType GhcPs -> Maybe ParsedType
parseType (L _ ty) =
  case ty of
    HsTyVar _ flag name -> Just $ TypeVar flag name
    HsListTy _ t -> TypeList <$> parseType t
    _ -> Nothing

{----- Compat / Expr -----}

mkExplicitList :: [LHsExpr GhcPs] -> HsExpr GhcPs
mkExplicitList = ExplicitList noExtField Nothing

mkExplicitTuple :: [HsTupArg GhcPs] -> Boxity -> HsExpr GhcPs
mkExplicitTuple = ExplicitTuple noAnn . map (L generatedSrcAnn)

xAppTypeE :: XAppTypeE GhcPs
xAppTypeE = noExtField

{----- Compat / Pat -----}

parsePat :: LPat GhcPs -> Maybe ParsedPat
parsePat (L _ pat) =
  case pat of
    VarPat _ name -> Just $ PatVar name
    ConPatIn name (PrefixCon args) -> PatPrefixCon name <$> mapM parsePat args
    LitPat _ (HsString _ s) -> Just $ PatLitString $ unpackFS s
    ParPat _ p -> parsePat p
    _ -> Nothing

{----- Backports -----}

type SrcAnn ann = SrcSpan
type SrcSpanAnn' a = SrcSpan
type LocatedN = Located

unLoc :: GenLocated l e -> e
unLoc (L _ e) = e

getLoc :: GenLocated l e -> l
getLoc (L l _) = l

getLocA :: Located e -> SrcSpan
getLocA = getLoc

mkHsApps :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkHsApps = foldl' mkHsApp

mkMatch :: HsMatchContext RdrName -> [LPat GhcPs] -> LHsExpr GhcPs -> HsLocalBinds GhcPs -> LMatch GhcPs (LHsExpr GhcPs)
mkMatch ctxt pats expr lbinds = GHC.mkMatch ctxt pats expr (L generatedSrcAnn lbinds)

noAnn :: NoExtField
noAnn = NoExtField

hsTypeToHsSigType :: LHsType GhcPs -> LHsSigType GhcPs
hsTypeToHsSigType = mkLHsSigType

hsTypeToHsSigWcType :: LHsType GhcPs -> LHsSigWcType GhcPs
hsTypeToHsSigWcType = mkLHsSigWcType

-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8492
thNameToGhcNameIO :: HscEnv -> IORef NameCache -> TH.Name -> IO (Maybe Name)
thNameToGhcNameIO hscEnv cache name =
  fmap fst
    . runCoreM
      hscEnv{hsc_NC = cache}
      (unused "cr_rule_base")
      (strict '.')
      (unused "cr_module")
      (strict mempty)
      (unused "cr_print_unqual")
      (unused "cr_loc")
    $ thNameToGhcName name
  where
    unused msg = error $ "unexpectedly used: " ++ msg

    -- marks fields that are strict, so we can't use `unused`
    strict = id
