{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC (
  module Test.Tasty.AutoCollect.GHC.Shim,

  -- * Output helpers
  showPpr,

  -- * Parsers
  parseLitStrPat,
  parseSigWcType,

  -- * Builders
  genFuncSig,
  genFuncDecl,
  lhsvar,
  mkHsVar,
  mkHsAppTypes,
  mkHsTyVar,
  mkLet,
  mkExprTypeSig,
  mkHsLitString,

  -- * Annotation utilities
  toSrcAnnA,
  getExportComments,

  -- * Located utilities
  genLoc,
  firstLocatedWhere,
  getSpanLine,

  -- * Name utilities
  mkRdrName,
  mkLRdrName,
  mkRdrNameType,
  mkLRdrNameType,
  fromRdrName,
) where

import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified GHC.Data.Strict as Strict
import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)

import Test.Tasty.AutoCollect.GHC.Shim hiding (
  mkHsAppTypes,
  mkLet,
  msg,
  showPpr,
 )
import Test.Tasty.AutoCollect.Utils.Text (withoutPrefix, withoutSuffix)

{----- Output helpers -----}

showPpr :: (Outputable a) => a -> String
showPpr = showSDocUnsafe . ppr

{----- Parsers ----}

parseLitStrPat :: LPat GhcPs -> Maybe String
parseLitStrPat = \case
  L _ (LitPat _ (HsString _ s)) -> Just (unpackFS s)
  _ -> Nothing

parseSigWcType :: LHsSigWcType GhcPs -> Maybe ParsedType
parseSigWcType (HsWC _ (L _ (HsSig _ _ ltype))) = parseType ltype

parseType :: LHsType GhcPs -> Maybe ParsedType
parseType (L _ ty) =
  case ty of
    HsTyVar _ flag name -> Just $ TypeVar flag name
    HsListTy _ t -> TypeList <$> parseType t
    _ -> Nothing

{----- Builders -----}

genFuncSig :: LocatedN RdrName -> LHsType GhcPs -> HsDecl GhcPs
genFuncSig funcName funcType =
  SigD noExtField
    . TypeSig noAnn [funcName]
    . hsTypeToHsSigWcType
    $ funcType

-- | Make simple function declaration of the form `<funcName> <funcArgs> = <funcBody> where <funcWhere>`
genFuncDecl :: LocatedN RdrName -> [LPat GhcPs] -> LHsExpr GhcPs -> Maybe (HsLocalBinds GhcPs) -> HsDecl GhcPs
genFuncDecl funcName funcArgs funcBody mFuncWhere =
  ValD NoExtField . mkFunBind generatedOrigin funcName $
    [ mkMatch (mkPrefixFunRhs funcName) funcArgs funcBody funcWhere
    ]
  where
    funcWhere = fromMaybe emptyLocalBinds mFuncWhere

lhsvar :: LocatedN RdrName -> LHsExpr GhcPs
lhsvar = genLoc . HsVar NoExtField

mkHsVar :: Name -> LHsExpr GhcPs
mkHsVar = lhsvar . genLoc . getRdrName

mkHsAppTypes :: LHsExpr GhcPs -> [LHsType GhcPs] -> LHsExpr GhcPs
mkHsAppTypes = foldl' (\e -> genLoc . mkHsAppType e)

mkHsTyVar :: Name -> LHsType GhcPs
mkHsTyVar = genLoc . HsTyVar noAnn NotPromoted . genLoc . getRdrName

mkLet :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkLet binds expr = HsLet noAnn (L NoTokenLoc HsTok) binds (L NoTokenLoc HsTok) expr

-- | mkExprTypeSig e t = [| $e :: $t |]
mkExprTypeSig :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkExprTypeSig e t =
  genLoc . ExprWithTySig noAnn e $
    HsWC NoExtField (hsTypeToHsSigType t)

mkHsLitString :: String -> LHsExpr GhcPs
mkHsLitString = genLoc . HsLit noAnn . mkHsString

{----- Annotation utilities -----}

toSrcAnnA :: RealSrcSpan -> SrcSpanAnnA
toSrcAnnA rss = SrcSpanAnn noAnn (RealSrcSpan rss Strict.Nothing)

-- | Get the contents of all comments in the given hsmodExports list.
getExportComments :: LocatedL [LIE GhcPs] -> [RealLocated String]
getExportComments = map fromLEpaComment . priorComments . epAnnComments . ann . getLoc
  where
    fromLEpaComment (L Anchor{anchor} EpaComment{ac_tok}) =
      L anchor $ (Text.unpack . Text.strip . unwrap) ac_tok
    unwrap = \case
      EpaDocComment doc -> Text.pack $ renderHsDocString doc
      EpaDocOptions s -> Text.pack s
      EpaLineComment s -> withoutPrefix "--" $ Text.pack s
      EpaBlockComment s -> withoutPrefix "{-" . withoutSuffix "-}" $ Text.pack s
      EpaEofComment -> ""

{----- Located utilities -----}

genLoc :: e -> GenLocated (SrcAnn ann) e
genLoc = L (SrcSpanAnn noAnn generatedSrcSpan)

firstLocatedWhere :: (Ord l) => (GenLocated l e -> Maybe a) -> [GenLocated l e] -> Maybe a
firstLocatedWhere f = listToMaybe . mapMaybe f . sortOn getLoc

getSpanLine :: SrcSpan -> String
getSpanLine loc =
  case srcSpanStart loc of
    RealSrcLoc srcLoc _ -> "line " ++ show (srcLocLine srcLoc)
    UnhelpfulLoc s -> unpackFS s

{----- Name utilities -----}

mkRdrName :: String -> RdrName
mkRdrName = mkRdrUnqual . mkOccName NameSpace.varName

mkLRdrName :: String -> LocatedN RdrName
mkLRdrName = genLoc . mkRdrName

mkRdrNameType :: String -> RdrName
mkRdrNameType = mkRdrUnqual . mkOccName NameSpace.tcName

mkLRdrNameType :: String -> LocatedN RdrName
mkLRdrNameType = genLoc . mkRdrNameType

fromRdrName :: LocatedN RdrName -> String
fromRdrName = occNameString . rdrNameOcc . unLoc
