{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC (
  module Test.Tasty.AutoCollect.GHC.Shim,

  -- * Output helpers
  showPpr,

  -- * Parsers
  parseLitStrPat,

  -- * Builders
  genFuncSig,
  genFuncDecl,
  lhsvar,
  mkHsVar,
  mkHsAppTypes,
  mkHsTyVar,
  mkExprTypeSig,
  mkHsLitString,

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
import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)

import Test.Tasty.AutoCollect.GHC.Shim

{----- Output helpers -----}

showPpr :: (Outputable a) => a -> String
showPpr = showSDocUnsafe . ppr

{----- Parsers ----}

parseLitStrPat :: LPat GhcPs -> Maybe String
parseLitStrPat = \case
  L _ (LitPat _ (HsString _ s)) -> Just (unpackFS s)
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
  ValD NoExtField . mkFunBind Generated funcName $
    [ mkMatch (mkPrefixFunRhs funcName) funcArgs funcBody funcWhere
    ]
  where
    funcWhere = fromMaybe emptyLocalBinds mFuncWhere

lhsvar :: LocatedN RdrName -> LHsExpr GhcPs
lhsvar = genLoc . HsVar NoExtField

mkHsVar :: Name -> LHsExpr GhcPs
mkHsVar = lhsvar . genLoc . getRdrName

mkHsAppTypes :: LHsExpr GhcPs -> [LHsType GhcPs] -> LHsExpr GhcPs
mkHsAppTypes = foldl' mkHsAppType

mkHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkHsAppType e t = genLoc $ HsAppType xAppTypeE e (HsWC noExtField t)

mkHsTyVar :: Name -> LHsType GhcPs
mkHsTyVar = genLoc . HsTyVar noAnn NotPromoted . genLoc . getRdrName

-- | mkExprTypeSig e t = [| $e :: $t |]
mkExprTypeSig :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkExprTypeSig e t =
  genLoc . ExprWithTySig noAnn e $
    HsWC NoExtField (hsTypeToHsSigType t)

mkHsLitString :: String -> LHsExpr GhcPs
mkHsLitString = genLoc . HsLit noAnn . mkHsString

{----- Located utilities -----}

genLoc :: e -> GenLocated (SrcAnn ann) e
genLoc = L generatedSrcAnn

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
