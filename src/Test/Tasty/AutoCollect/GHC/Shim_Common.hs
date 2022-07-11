{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim_Common (
  ParsedDecl (..),
  FuncSingleDef (..),
  FuncGuardedBody (..),
  ParsedType (..),
  ParsedPat (..),
  ConstructorDetails (..),
) where

import GHC.Hs
#if __GLASGOW_HASKELL__ == 810
import BasicTypes (Boxity, PromotionFlag)
import RdrName (RdrName)
import SrcLoc (Located)
#elif __GLASGOW_HASKELL__ == 900
import GHC.Types.Basic (Boxity, PromotionFlag)
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (Located)
#elif __GLASGOW_HASKELL__ == 902
import GHC.Types.Basic (Boxity, PromotionFlag)
import GHC.Types.Name.Reader (RdrName)
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

data ParsedPat
  = PatWildCard
  | PatVar (LocatedN RdrName)
  | PatLazy
  | PatAs
  | PatParens ParsedPat
  | PatBang
  | PatList [ParsedPat]
  | PatTuple [ParsedPat] Boxity
  | PatSum
  | PatConstructor (LocatedN RdrName) ConstructorDetails
  | PatView
  | PatSplice (HsSplice GhcPs)
  | PatLiteral (HsLit GhcPs)
  | PatOverloadedLit (Located (HsOverLit GhcPs))
  | PatNPlusK
  | PatTypeSig ParsedPat (LHsType GhcPs)

data ConstructorDetails
  = ConstructorPrefix [LHsType GhcPs] [ParsedPat]
  | ConstructorRecord (HsRecFields GhcPs ParsedPat)
  | ConstructorInfix ParsedPat ParsedPat
