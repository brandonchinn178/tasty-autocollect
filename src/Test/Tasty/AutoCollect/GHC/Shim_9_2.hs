{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_2 (
  -- * Re-exports
  module X,

  -- * Compat

  -- ** Plugin
  setKeepRawTokenStream,
  withParsedResultModule,

  -- ** Annotations
  generatedSrcAnn,
  getExportComments,
  toSrcAnnA,

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
  thNameToGhcNameIO,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X hiding (comment, mkHsAppType, mkHsAppTypes)
import GHC.Plugins as X hiding (
  AnnBind (..),
  AnnExpr' (..),
  getHscEnv,
  mkLet,
  showPpr,
  varName,
 )
import GHC.Types.Name.Cache as X (NameCache)

import Data.IORef (IORef)
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH

import Test.Tasty.AutoCollect.GHC.Shim_Common
import Test.Tasty.AutoCollect.Utils.Text

{----- Compat / Plugin -----}

setKeepRawTokenStream :: Plugin -> Plugin
setKeepRawTokenStream plugin =
  plugin
    { driverPlugin = \_ env ->
        pure
          env
            { hsc_dflags = hsc_dflags env `gopt_set` Opt_KeepRawTokenStream
            }
    }

withParsedResultModule :: HsParsedModule -> (HsParsedModule -> HsParsedModule) -> HsParsedModule
withParsedResultModule = flip ($)

{----- Compat / Annotations -----}

-- | Get the contents of all comments in the given hsmodExports list.
getExportComments :: HsParsedModule -> LocatedL [LIE GhcPs] -> [RealLocated String]
getExportComments _ = map fromLEpaComment . priorComments . epAnnComments . ann . getLoc
  where
    fromLEpaComment (L Anchor{anchor} EpaComment{ac_tok}) =
      L anchor $ (Text.unpack . Text.strip . unwrap) ac_tok
    unwrap = \case
      EpaDocCommentNext s -> withoutPrefix "-- |" $ Text.pack s
      EpaDocCommentPrev s -> withoutPrefix "-- ^" $ Text.pack s
      EpaDocCommentNamed s -> withoutPrefix "-- $" $ Text.pack s
      EpaDocSection _ s -> Text.pack s
      EpaDocOptions s -> Text.pack s
      EpaLineComment s -> withoutPrefix "--" $ Text.pack s
      EpaBlockComment s -> withoutPrefix "{-" . withoutSuffix "-}" $ Text.pack s
      EpaEofComment -> ""

generatedSrcAnn :: SrcAnn ann
generatedSrcAnn = SrcSpanAnn noAnn generatedSrcSpan

toSrcAnnA :: RealSrcSpan -> SrcSpanAnnA
toSrcAnnA rss = SrcSpanAnn noAnn (RealSrcSpan rss Nothing)

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

{----- Compat / Type -----}

parseSigWcType :: LHsSigWcType GhcPs -> Maybe ParsedType
parseSigWcType (HsWC _ (L _ (HsSig _ _ ltype))) = parseType ltype

parseType :: LHsType GhcPs -> Maybe ParsedType
parseType (L _ ty) =
  case ty of
    HsTyVar _ flag name -> Just $ TypeVar flag name
    HsListTy _ t -> TypeList <$> parseType t
    _ -> Nothing

{----- Compat / Expr -----}

mkExplicitList :: [LHsExpr GhcPs] -> HsExpr GhcPs
mkExplicitList = ExplicitList noAnn

mkExplicitTuple :: [HsTupArg GhcPs] -> Boxity -> HsExpr GhcPs
mkExplicitTuple = ExplicitTuple noAnn

mkLet :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkLet binds expr = HsLet noAnn binds expr

xAppTypeE :: XAppTypeE GhcPs
xAppTypeE = generatedSrcSpan

{----- Backports -----}

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
