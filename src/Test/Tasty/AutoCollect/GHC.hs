{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC (
  -- * Parsers
  getBlockComment,

  -- * Builders
  genFuncSig,
  genFuncDecl,

  -- * Located utilities
  genLoc,
  firstLocatedWhere,
  getSpanLine,

  -- * Name utilities
  mkRdrName,
  mkRdrNameType,
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
import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)
import qualified Language.Haskell.TH as TH

{----- Parsers -----}

getBlockComment :: RealLocated AnnotationComment -> Maybe String
getBlockComment = \case
  L _ (AnnBlockComment s) ->
    fmap (Text.unpack . Text.strip) . Text.stripSuffix "-}" <=< Text.stripPrefix "{-" . Text.pack $ s
  _ -> Nothing

{----- Builders -----}

genFuncSig :: Located RdrName -> LHsType GhcPs -> HsDecl GhcPs
genFuncSig funcName funcType =
  SigD NoExtField
    . TypeSig NoExtField [funcName]
    . mkLHsSigWcType
    $ funcType

-- | Make simple function declaration of the form `<funcName> <funcArgs> = <funcBody> where <funcWhere>`
genFuncDecl :: Located RdrName ->  [LPat GhcPs] -> LHsExpr GhcPs -> Maybe (LHsLocalBinds GhcPs) -> HsDecl GhcPs
genFuncDecl funcName funcArgs funcBody mFuncWhere =
  ValD NoExtField . mkFunBind Generated funcName $
    [ mkMatch (mkPrefixFunRhs funcName) funcArgs funcBody funcWhere
    ]
  where
    funcWhere = fromMaybe (genLoc emptyLocalBinds) mFuncWhere

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

mkRdrNameType :: String -> Located RdrName
mkRdrNameType = genLoc . mkRdrUnqual . mkOccName NameSpace.tcName

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
