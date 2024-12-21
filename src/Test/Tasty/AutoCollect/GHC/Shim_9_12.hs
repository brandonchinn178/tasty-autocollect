{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_12 (
  -- * Re-exports
  module X,

  -- * Compat

  -- ** Decl
  generatedOrigin,

  -- ** Expr
  mkHsAppType,
  mkLet,
  mkHsLitString,
  mkPrefixFunRhs,
  toMatchArgs,
  fromMatchArgs,

  -- ** Name
  mkIEVar,

  -- ** Annotations + Located
  getEpAnn,
  toSrcAnnA,
  genLoc,
  epaCommentTokText,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X hiding (mkHsAppType, mkPrefixFunRhs)
import GHC.Plugins as X hiding (
  AnnBind (..),
  AnnExpr' (..),
  getHscEnv,
  mkLet,
 )
import GHC.Types.Name.Cache as X (NameCache)

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Hs qualified as GHC

import Test.Tasty.AutoCollect.Utils.Text (withoutPrefix, withoutSuffix)

{----- Compat / Decl -----}

generatedOrigin :: Origin
generatedOrigin = Generated OtherExpansion DoPmc

{----- Compat / Expr -----}

mkHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> HsExpr GhcPs
mkHsAppType e t = HsAppType NoEpTok e (HsWC noExtField t)

mkLet :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkLet binds expr = HsLet (NoEpTok, NoEpTok) binds expr

mkHsLitString :: String -> LHsExpr GhcPs
mkHsLitString = genLoc . HsLit noExtField . mkHsString

mkPrefixFunRhs :: fn -> AnnFunRhs -> HsMatchContext fn
mkPrefixFunRhs = GHC.mkPrefixFunRhs

toMatchArgs :: LocatedE [LPat GhcPs] -> LocatedE [LPat GhcPs]
toMatchArgs = id

fromMatchArgs :: LocatedE [LPat GhcPs] -> LocatedE [LPat GhcPs]
fromMatchArgs = id

{----- Compat / Name -----}

mkIEVar :: LIEWrappedName GhcPs -> IE GhcPs
mkIEVar n = IEVar Nothing n Nothing

{----- Compat / Annotations + Located -----}

getEpAnn :: GenLocated (EpAnn ann) e -> EpAnn ann
getEpAnn = getLoc

toSrcAnnA :: RealSrcSpan -> SrcSpanAnnA
toSrcAnnA rss = EpAnn (realSpanAsAnchor rss) noAnn (EpaComments [])

genLoc :: (NoAnn ann) => e -> GenLocated ann e
genLoc = L noAnn

epaCommentTokText :: EpaCommentTok -> Text
epaCommentTokText = \case
  EpaDocComment doc -> Text.pack $ renderHsDocString doc
  EpaDocOptions s -> Text.pack s
  EpaLineComment s -> withoutPrefix "--" $ Text.pack s
  EpaBlockComment s -> withoutPrefix "{-" . withoutSuffix "-}" $ Text.pack s
