{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_8 (
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
  LocatedE,
  LocatedLI,
  getEpAnn,
  toSrcAnnA,
  genLoc,
  epaCommentTokText,
  epaLocationRealSrcSpan,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X hiding (
  epaLocationRealSrcSpan,
  mkHsAppType,
  mkPrefixFunRhs,
 )
import GHC.Plugins as X hiding (
  AnnBind (..),
  AnnExpr' (..),
  getHscEnv,
  mkLet,
 )
import GHC.Types.Name.Cache as X (NameCache)

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Data.Strict qualified as Strict
import GHC.Hs qualified as GHC

import Test.Tasty.AutoCollect.Utils.Text (withoutPrefix, withoutSuffix)

{----- Compat / Decl -----}

generatedOrigin :: Origin
generatedOrigin = Generated DoPmc

{----- Compat / Expr -----}

mkHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> HsExpr GhcPs
mkHsAppType e t = HsAppType noExtField e (L NoTokenLoc HsTok) (HsWC noExtField t)

mkLet :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkLet binds expr = HsLet noAnn (L NoTokenLoc HsTok) binds (L NoTokenLoc HsTok) expr

mkHsLitString :: String -> LHsExpr GhcPs
mkHsLitString = genLoc . HsLit noAnn . mkHsString

mkPrefixFunRhs :: LIdP GhcPs -> EpAnn () -> HsMatchContext GhcPs
mkPrefixFunRhs fn _ = GHC.mkPrefixFunRhs fn

toMatchArgs :: LocatedE [LPat GhcPs] -> [LPat GhcPs]
toMatchArgs = unLoc

fromMatchArgs :: [LPat GhcPs] -> LocatedE [LPat GhcPs]
fromMatchArgs = genLoc

{----- Compat / Name -----}

mkIEVar :: LIEWrappedName GhcPs -> IE GhcPs
mkIEVar = IEVar Nothing

{----- Compat / Annotations + Located -----}

type LocatedE = LocatedL
type LocatedLI = LocatedL

getEpAnn :: GenLocated (SrcAnn ann) e -> EpAnn ann
getEpAnn = ann . getLoc

toSrcAnnA :: RealSrcSpan -> SrcSpanAnnA
toSrcAnnA rss = SrcSpanAnn noAnn (RealSrcSpan rss Strict.Nothing)

genLoc :: e -> GenLocated (SrcAnn ann) e
genLoc = L (SrcSpanAnn noAnn generatedSrcSpan)

epaCommentTokText :: EpaCommentTok -> Text
epaCommentTokText = \case
  EpaDocComment doc -> Text.pack $ renderHsDocString doc
  EpaDocOptions s -> Text.pack s
  EpaLineComment s -> withoutPrefix "--" $ Text.pack s
  EpaBlockComment s -> withoutPrefix "{-" . withoutSuffix "-}" $ Text.pack s
  EpaEofComment -> ""

epaLocationRealSrcSpan :: Anchor -> RealSrcSpan
epaLocationRealSrcSpan = anchor
