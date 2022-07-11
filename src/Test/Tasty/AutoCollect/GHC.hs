{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC (
  module Test.Tasty.AutoCollect.GHC.Shim,

  -- * Output helpers
  showPpr,

  -- * Builders
  genFuncSig,
  genFuncDecl,
  lhsvar,
  mkHsAppTypes,
  mkHsTyVar,
  mkExprTypeSig,

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
  thNameToGhcNameIO,
) where

import Data.Foldable (foldl')
import Data.IORef (IORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Language.Haskell.TH as TH

import Test.Tasty.AutoCollect.GHC.Shim

{----- Output helpers -----}

showPpr :: Outputable a => a -> String
showPpr = showSDocUnsafe . ppr

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

mkHsAppTypes :: LHsExpr GhcPs -> [LHsType GhcPs] -> LHsExpr GhcPs
mkHsAppTypes = foldl' mkHsAppType

mkHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkHsAppType e t = genLoc $ HsAppType xAppTypeE e (HsWC noExtField t)

mkHsTyVar :: Name -> LHsType GhcPs
mkHsTyVar = genLoc . HsTyVar noAnn NotPromoted . genLoc . getRdrName

-- | mkExprTypeSig <e> <t> = (<e> :: <t>)
mkExprTypeSig :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
mkExprTypeSig e t =
  genLoc . ExprWithTySig noAnn e $
    HsWC NoExtField (hsTypeToHsSigType t)

{----- Located utilities -----}

genLoc :: e -> GenLocated (SrcAnn ann) e
genLoc = L generatedSrcAnn

firstLocatedWhere :: Ord l => (GenLocated l e -> Maybe a) -> [GenLocated l e] -> Maybe a
firstLocatedWhere f = listToMaybe . mapMaybe f . sortOn getLoc

getSpanLine :: GenLocated (SrcSpanAnn' a) e -> String
getSpanLine loc =
  case srcSpanStart $ getLocA loc of
    Right srcLoc -> "line " ++ show (srcLocLine srcLoc)
    Left s -> s

{----- Name utilities -----}

mkRdrName :: String -> RdrName
mkRdrName = mkRdrUnqual . mkOccNameVar

mkLRdrName :: String -> LocatedN RdrName
mkLRdrName = genLoc . mkRdrName

mkRdrNameType :: String -> RdrName
mkRdrNameType = mkRdrUnqual . mkOccNameTC

mkLRdrNameType :: String -> LocatedN RdrName
mkLRdrNameType = genLoc . mkRdrNameType

fromRdrName :: LocatedN RdrName -> String
fromRdrName = occNameString . rdrNameOcc . unLoc

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
