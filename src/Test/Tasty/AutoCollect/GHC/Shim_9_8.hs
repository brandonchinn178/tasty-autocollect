{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_8 (
  -- * Re-exports
  module X,

  -- * Compat

  -- ** Decl
  parseDecl,
  generatedOrigin,

  -- ** Expr
  mkHsAppType,
  mkLet,
  mkHsLitString,

  -- ** Name
  mkIEVar,
  mkIEName,

  -- ** Annotations + Located
  getEpAnn,
  toSrcAnnA,
  genLoc,
  epaCommentTokText,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X hiding (mkHsAppType)
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

import Test.Tasty.AutoCollect.GHC.Shim_Common
import Test.Tasty.AutoCollect.Utils.Text (withoutPrefix, withoutSuffix)

{----- Compat / Decl -----}

parseDecl :: LHsDecl GhcPs -> Maybe ParsedDecl
parseDecl (L _ decl) =
  case decl of
    SigD _ (TypeSig _ names ty) -> Just $ FuncSig names ty
    ValD _ (FunBind _ name MG{mg_alts = L _ matches}) ->
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

generatedOrigin :: Origin
generatedOrigin = Generated DoPmc

{----- Compat / Expr -----}

mkHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> HsExpr GhcPs
mkHsAppType e t = HsAppType noExtField e (L NoTokenLoc HsTok) (HsWC noExtField t)

mkLet :: HsLocalBinds GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkLet binds expr = HsLet noAnn (L NoTokenLoc HsTok) binds (L NoTokenLoc HsTok) expr

mkHsLitString :: String -> LHsExpr GhcPs
mkHsLitString = genLoc . HsLit noAnn . mkHsString

{----- Compat / Name -----}

mkIEVar :: LIEWrappedName GhcPs -> IE GhcPs
mkIEVar = IEVar Nothing

mkIEName :: LocatedN RdrName -> IEWrappedName GhcPs
mkIEName = IEName noExtField

{----- Compat / Annotations + Located -----}

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
