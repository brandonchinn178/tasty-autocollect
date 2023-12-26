{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim_Common (
  ParsedDecl (..),
  FuncSingleDef (..),
  FuncGuardedBody (..),
  ParsedType (..),
) where

import GHC.Hs
#if __GLASGOW_HASKELL__ < 906
import GHC.Types.Basic (PromotionFlag)
#endif
import GHC.Types.Name.Reader (RdrName)

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
