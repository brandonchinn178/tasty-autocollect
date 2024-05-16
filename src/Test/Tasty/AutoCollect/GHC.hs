{-# LANGUAGE CPP #-}
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
  mkExprTypeSig,

  -- * Annotation utilities
  getExportComments,

  -- * Located utilities
  firstLocatedWhere,
  getSpanLine,

  -- * Name utilities
  mkRdrName,
  mkLRdrName,
  mkRdrNameType,
  mkLRdrNameType,
  fromRdrName,
) where

#if __GLASGOW_HASKELL__ < 910
import Data.Foldable (foldl')
#endif
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text qualified as Text
import GHC.Types.Name.Occurrence qualified as NameSpace (tcName, varName)

import Test.Tasty.AutoCollect.GHC.Shim hiding (
  mkHsAppTypes,
  msg,
  showPpr,
 )

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

-- | mkExprTypeSig e t = [| $e :: $t |]
mkExprTypeSig :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkExprTypeSig e t =
  genLoc . ExprWithTySig noAnn e $
    HsWC NoExtField (hsTypeToHsSigType t)

{----- Annotation utilities -----}

-- | Get the contents of all comments in the given hsmodExports list.
getExportComments :: LocatedL [LIE GhcPs] -> [RealLocated String]
getExportComments = map fromLEpaComment . priorComments . epAnnComments . getEpAnn
  where
    fromLEpaComment (L ann EpaComment{ac_tok}) =
      L (anchor ann) $ (Text.unpack . Text.strip . epaCommentTokText) ac_tok

{----- Located utilities -----}

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
