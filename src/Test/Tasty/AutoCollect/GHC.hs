{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC (
  -- * Parsers
  getBlockComment,

  -- * Builders
  genHsWC,
  genFuncSig,
  genFuncDecl,
  exprApply,

  -- * Located utilities
  genLoc,
  firstLocatedWhere,
  getSpanLine,

  -- * Name utilities
  mkRdrName,
  fromRdrName,
  thNameToGhcNameIO,
) where

import Control.Monad ((<=<))
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import GHC.IORef (IORef)
import GHC.Driver.Main (newHscEnv)
import GHC.Hs
import GHC.Parser.Annotation (AnnotationComment (..))
import GHC.Plugins
import GHC.SysTools (initSysTools)
import GHC.SysTools.BaseDir (findTopDir)
import GHC.Types.Name.Cache (NameCache)
import qualified GHC.Types.Name.Occurrence as NameSpace (varName)
import qualified Language.Haskell.TH as TH

{----- Parsers -----}

getBlockComment :: RealLocated AnnotationComment -> Maybe String
getBlockComment = \case
  L _ (AnnBlockComment s) ->
    fmap (Text.unpack . Text.strip) . Text.stripSuffix "-}" <=< Text.stripPrefix "{-" . Text.pack $ s
  _ -> Nothing

{----- Builders -----}

genHsWC :: LHsType GhcPs -> LHsSigWcType GhcPs
genHsWC = HsWC NoExtField . HsIB NoExtField

genFuncSig :: Located RdrName -> LHsType GhcPs -> HsDecl GhcPs
genFuncSig funcName funcType =
  SigD NoExtField
    . TypeSig NoExtField [funcName]
    . genHsWC
    $ funcType

-- | Make simple function declaration of the form `<funcName> <funcArgs> = <funcBody> where <funcWhere>`
genFuncDecl :: Located RdrName ->  [LPat GhcPs] -> LHsExpr GhcPs -> Maybe (LHsLocalBinds GhcPs) -> HsDecl GhcPs
genFuncDecl funcName funcArgs funcBody mFuncWhere =
  (\body -> ValD NoExtField $ FunBind NoExtField funcName body [])
    . (\match -> MG NoExtField (genLoc [genLoc match]) Generated)
    . Match NoExtField (FunRhs funcName Prefix NoSrcStrict) funcArgs
    . (\grhs -> GRHSs NoExtField [genLoc grhs] funcWhere)
    $ GRHS NoExtField [] funcBody
  where
    funcWhere = fromMaybe (genLoc $ EmptyLocalBinds NoExtField) mFuncWhere

-- | Apply the given [f, x1, x2, x3] as `(((f x1) x2) x3)`.
exprApply :: [LHsExpr GhcPs] -> LHsExpr GhcPs
exprApply = foldl1 (\f x -> genLoc $ HsApp NoExtField f x)

{----- Located utilities -----}

genLoc :: e -> Located e
genLoc = L generatedSrcSpan

firstLocatedWhere :: Ord l => (GenLocated l e -> Maybe a) -> [GenLocated l e] -> Maybe a
firstLocatedWhere f = listToMaybe . mapMaybe f . sortOn getLoc

getSpanLine :: Located a -> String
getSpanLine loc =
  case srcSpanStart $ getLoc loc of
    RealSrcLoc srcLoc _ -> "line " ++ show (srcLocLine srcLoc)
    UnhelpfulLoc s -> unpackFS s

{----- Name utilities -----}

mkRdrName :: String -> Located RdrName
mkRdrName = genLoc . mkRdrUnqual . mkOccName NameSpace.varName

fromRdrName :: Located RdrName -> String
fromRdrName = occNameString . rdrNameOcc . unLoc

-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8492
thNameToGhcNameIO :: IORef NameCache -> TH.Name -> IO (Maybe Name)
thNameToGhcNameIO cache name = do
  -- https://gitlab.haskell.org/ghc/ghc/-/blob/b5590fff75496356b1817adc9de1f2d361a70dc5/compiler/GHC/Driver/Main.hs#L306-317
  dir <- findTopDir Nothing
  sysSettings <- initSysTools dir
  let dflags = defaultDynFlags sysSettings (unused "DynFlags.llvmConfig")
  hscEnv <- newHscEnv dflags

  fmap fst
    . runCoreM
        hscEnv{hsc_NC = cache}
        (unused "cr_rule_base")
        (strict '.')
        (unused "cr_module")
        (strict mempty)
        (unused "cr_print_unqual")
        (unused "cr_loc")
    $ thNameToGhcName name
  where
    unused msg = error $ "unexpectedly used: " ++ msg

    -- marks fields that are strict, so we can't use `unused`
    strict = id
