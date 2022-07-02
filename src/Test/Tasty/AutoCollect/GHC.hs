{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GHC (
  -- * Parsers
  getCommentContent,

  -- * Builders
  genFuncSig,
  genFuncDecl,

  -- * Located utilities
  genLoc,
  firstLocatedWhere,
  getSpanLine,

  -- * Name utilities
  mkRdrName,
  mkLRdrName,
  mkRdrNameType,
  mkLRdrNameType,
  fromRdrName,
  thNameToGhcNameIO,
) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as Text
import GHC.Hs
import GHC.IORef (IORef)
import GHC.Parser.Annotation (AnnotationComment (..))
import GHC.Plugins
import GHC.Types.Name.Cache (NameCache)
import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)
import qualified Language.Haskell.TH as TH

import Test.Tasty.AutoCollect.Utils.Text

{----- Parsers -----}

getCommentContent :: RealLocated AnnotationComment -> String
getCommentContent = Text.unpack . Text.strip . unwrapComment . unLoc
  where
    unwrapComment = \case
      AnnDocCommentNext s -> withoutPrefix "-- |" $ Text.pack s
      AnnDocCommentPrev s -> withoutPrefix "-- ^" $ Text.pack s
      AnnDocCommentNamed s -> withoutPrefix "-- $" $ Text.pack s
      AnnDocSection _ s -> Text.pack s
      AnnDocOptions s -> Text.pack s
      AnnLineComment s -> withoutPrefix "--" $ Text.pack s
      AnnBlockComment s -> withoutPrefix "{-" . withoutSuffix "-}" $ Text.pack s

{----- Builders -----}

genFuncSig :: Located RdrName -> LHsType GhcPs -> HsDecl GhcPs
genFuncSig funcName funcType =
  SigD NoExtField
    . TypeSig NoExtField [funcName]
    . mkLHsSigWcType
    $ funcType

-- | Make simple function declaration of the form `<funcName> <funcArgs> = <funcBody> where <funcWhere>`
genFuncDecl :: Located RdrName -> [LPat GhcPs] -> LHsExpr GhcPs -> Maybe (LHsLocalBinds GhcPs) -> HsDecl GhcPs
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

mkRdrName :: String -> RdrName
mkRdrName = mkRdrUnqual . mkOccName NameSpace.varName

mkLRdrName :: String -> Located RdrName
mkLRdrName = genLoc . mkRdrName

mkRdrNameType :: String -> RdrName
mkRdrNameType = mkRdrUnqual . mkOccName NameSpace.tcName

mkLRdrNameType :: String -> Located RdrName
mkLRdrNameType = genLoc . mkRdrNameType

fromRdrName :: Located RdrName -> String
fromRdrName = occNameString . rdrNameOcc . unLoc

-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8492
thNameToGhcNameIO :: HscEnv -> IORef NameCache -> TH.Name -> IO (Maybe Name)
thNameToGhcNameIO hscEnv cache name =
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
