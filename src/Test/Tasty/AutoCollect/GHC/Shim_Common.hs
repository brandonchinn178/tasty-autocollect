{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim_Common (
  ParsedDecl (..),
  FuncSingleDef (..),
  FuncGuardedBody (..),
  ParsedType (..),
  ParsedPat (..),
) where

import GHC.Hs
#if __GLASGOW_HASKELL__ == 810
import BasicTypes (PromotionFlag)
import RdrName (RdrName)
import SrcLoc (Located)
#elif __GLASGOW_HASKELL__ == 900
import GHC.Types.Basic (PromotionFlag)
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (Located)
#elif __GLASGOW_HASKELL__ == 902
import GHC.Types.Basic (PromotionFlag)
import GHC.Types.Name.Reader (RdrName)
#elif __GLASGOW_HASKELL__ == 904
import GHC.Types.Basic (PromotionFlag)
import GHC.Types.Name.Reader (RdrName)
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

data ParsedPat
  = PatVar (LocatedN RdrName)
  | PatPrefixCon (LocatedN RdrName) [ParsedPat]
  | PatLitString String
