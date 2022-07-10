{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim_Common (
  ParsedDecl (..),
  FuncSingleDef (..),
  FuncGuardedBody (..),
  ParsedPat (..),
) where

import GHC.Hs
#if __GLASGOW_HASKELL__ == 810
import ApiAnnotation (LRdrName)
import BasicTypes (Boxity)
import SrcLoc (Located)
#elif __GLASGOW_HASKELL__ == 900
import GHC.Parser.Annotation (LRdrName)
import GHC.Types.Basic (Boxity)
import GHC.Types.SrcLoc (Located)
#endif

data ParsedDecl
  = FuncSig [LRdrName] (LHsSigWcType GhcPs)
  | FuncDef LRdrName [Located FuncSingleDef]

data FuncSingleDef = FuncSingleDef
  { funcDefArgs :: [LPat GhcPs]
  , funcDefGuards :: [FuncGuardedBody]
  , funcDefWhereClause :: LHsLocalBinds GhcPs
  }

data FuncGuardedBody = FuncGuardedBody
  { funcDefBodyGuards :: [GuardLStmt GhcPs]
  , funcDefBody :: LHsExpr GhcPs
  }

data ParsedPat
  = PatWildCard
  | PatVar LRdrName
  | PatLazy
  | PatAs
  | PatParens ParsedPat
  | PatBang
  | PatList [ParsedPat]
  | PatTuple [ParsedPat] Boxity
  | PatSum
  | PatConstructor LRdrName (HsConDetails ParsedPat (HsRecFields GhcPs ParsedPat))
  | PatView
  | PatSplice (HsSplice GhcPs)
  | PatLiteral (HsLit GhcPs)
  | PatOverloadedLit (Located (HsOverLit GhcPs))
  | PatNPlusK
  | PatTypeSig ParsedPat (LHsType GhcPs)
