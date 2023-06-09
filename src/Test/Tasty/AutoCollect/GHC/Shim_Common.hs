{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim_Common (
  ParsedDecl (..),
  FuncSingleDef (..),
  FuncGuardedBody (..),
  ParsedType (..),
) where

import GHC.Hs
import GHC.Types.Basic (PromotionFlag)
import GHC.Types.Name.Reader (RdrName)
#if __GLASGOW_HASKELL__ < 902
import GHC.Types.SrcLoc (Located)
#endif

#if __GLASGOW_HASKELL__ < 902
type LocatedA = Located
type LocatedN = Located
#endif

data ParsedDecl
  = FuncSig [LocatedN RdrName] (LHsSigWcType GhcPs)
  | FuncDef (LocatedN RdrName) [LocatedA FuncSingleDef]

data FuncSingleDef = FuncSingleDef
  { funcDefArgs :: [LPat GhcPs]
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
