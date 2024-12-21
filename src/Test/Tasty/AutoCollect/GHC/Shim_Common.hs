{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim_Common (
  ParsedDecl (..),
  FuncSingleDef (..),
  FuncGuardedBody (..),
  ParsedType (..),
) where

import GHC.Hs
import GHC.Types.Name.Reader (RdrName)

data ParsedDecl
  = FuncSig [LocatedN RdrName] (LHsSigWcType GhcPs)
  | FuncDef (LocatedN RdrName) [LocatedA FuncSingleDef]

#if __GLASGOW_HASKELL__ < 910
type LocatedE = LocatedL
#endif

data FuncSingleDef = FuncSingleDef
  { funcDefArgs :: LocatedE [LPat GhcPs]
  , funcDefGuards :: [FuncGuardedBody]
  , funcDefWhereClause :: HsLocalBinds GhcPs
  }

data FuncGuardedBody = FuncGuardedBody
  { funcDefBodyGuards :: [GuardLStmt GhcPs]
  , funcDefBody :: LHsExpr GhcPs
  }

data ParsedType
  = TypeVar PromotionFlag (LocatedN RdrName)
  | TypeList ParsedType
