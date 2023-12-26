{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.GHC.Shim_9_6 (
  -- * Re-exports
  module X,

  -- * Compat

  -- ** Decl
  parseDecl,
  generatedOrigin,

  -- ** Expr
  mkHsAppType,

  -- ** Name
  mkIEVar,
  mkIEName,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X hiding (mkHsAppType)
import GHC.Plugins as X hiding (
  AnnBind (..),
  AnnExpr' (..),
  getHscEnv,
 )
import GHC.Types.Name.Cache as X (NameCache)

import Test.Tasty.AutoCollect.GHC.Shim_Common

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
generatedOrigin = Generated

{----- Compat / Expr -----}

mkHsAppType :: LHsExpr GhcPs -> LHsType GhcPs -> HsExpr GhcPs
mkHsAppType e t = HsAppType noExtField e (L NoTokenLoc HsTok) (HsWC noExtField t)

{----- Compat / Name -----}

mkIEVar :: LIEWrappedName GhcPs -> IE GhcPs
mkIEVar = IEVar noExtField

mkIEName :: LocatedN RdrName -> IEWrappedName GhcPs
mkIEName = IEName noExtField
