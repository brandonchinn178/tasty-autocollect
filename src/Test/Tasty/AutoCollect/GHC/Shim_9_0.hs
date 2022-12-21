{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_0 (
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
  mkLet,
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
  thNameToGhcNameIO,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X hiding (mkHsAppType, mkHsAppTypes, mkMatch)
import GHC.Parser.Annotation as X (AnnotationComment (..))
import GHC.Plugins as X hiding (getHscEnv, mkLet, showPpr, varName)
import GHC.Types.Name.Cache as X (NameCache)

import Data.IORef (IORef)
import qualified Data.Text as Text
import qualified GHC.Hs.Utils as GHC (mkMatch)
import GHC.Parser.Annotation (getAnnotationComments)
import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)
import qualified Language.Haskell.TH as TH

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

{----- Compat / Expr -----}

mkExplicitList :: [LHsExpr GhcPs] -> HsExpr GhcPs
mkExplicitList = ExplicitList noExtField Nothing

mkExplicitTuple :: [HsTupArg GhcPs] -> Boxity -> HsExpr GhcPs
mkExplicitTuple = ExplicitTuple noAnn . map (L generatedSrcAnn)

mkLet :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkLet binds expr = HsLet noExtField (L generatedSrcAnn binds) expr

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
