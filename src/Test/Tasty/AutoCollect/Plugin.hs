{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import Control.Monad ((>=>))
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Hs (
  HsModule (..),
  IE (..),
  IEWrappedName (..),
  NoExtField (..),
 )
import GHC.Plugins
import GHC.Parser.Annotation (
  AnnotationComment (..),
  ApiAnns (..),
  getAnnotationComments,
 )
import qualified GHC.Types.Name.Occurrence as NameSpace (varName)

plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , parsedResultAction = \_ _ modl -> do
        return $
          case getModuleType modl of
            Just ModuleMain -> transformMainModule modl
            Just ModuleTest -> transformTestModule modl
            Nothing -> modl
    }

{----- AutoCollectAnn -----}

-- | A tasty-autocollect annotation.
data AutoCollectAnn
  = AutoCollectMain
  | AutoCollectTest
  | AutoCollectTestExport

parseAutoCollectAnn :: RealLocated AnnotationComment -> Maybe AutoCollectAnn
parseAutoCollectAnn = \case
  L _ (AnnBlockComment s) ->
    case map toLower s of
      "{- autocollect.main -}" -> Just AutoCollectMain
      "{- autocollect.test -}" -> Just AutoCollectTest
      "{- autocollect.test.export -}" -> Just AutoCollectTestExport
      _ -> Nothing
  _ -> Nothing

{----- ModuleType -----}

-- | The type of module being compiled.
data ModuleType = ModuleMain | ModuleTest

getModuleType :: HsParsedModule -> Maybe ModuleType
getModuleType = firstLocatedWhere parseModuleType . apiAnnRogueComments . hpm_annotations

parseModuleType :: RealLocated AnnotationComment -> Maybe ModuleType
parseModuleType = parseAutoCollectAnn >=> \case
  AutoCollectMain -> Just ModuleMain
  AutoCollectTest -> Just ModuleTest
  _ -> Nothing

{----- Transform modules -----}

transformMainModule :: HsParsedModule -> HsParsedModule
transformMainModule = trace "transformMainModule"

{- |
Transforms a test module of the form

@
{- AUTOCOLLECT.TEST -}
module MyTest (
  foo,
  {- AUTOCOLLECT.TEST.export -}
  bar,
) where

testCase :: <type>
testCase <name> <other args> = <test>
@

to the equivalent of

@
module MyTest (
  foo,
  tests,
  bar,
) where

tests :: [TestTree]
tests = [test1]

test1 :: TestTree
test1 =
  testCase <name> <other args> (<test> :: <type>)
@
-}
transformTestModule :: HsParsedModule -> HsParsedModule
transformTestModule parsedModl = parsedModl{hpm_module = updateModule <$> hpm_module parsedModl}
  where
    getCommentsAt = getAnnotationComments (hpm_annotations parsedModl)

    updateModule modl =
      modl
        { hsmodExports = updateExports <$> hsmodExports modl
        }

    -- Replace "{- AUTOCOLLECT.TEST.export -}" with `tests` in the export list
    updateExports loc
      | RealSrcSpan realSrcSpan _ <- getLoc loc
      , Just exportSpan <- firstLocatedWhere getTestExportAnnSrcSpan (getCommentsAt realSrcSpan) =
          (L (RealSrcSpan exportSpan Nothing) exportIE :) <$> loc
      | otherwise =
          loc
    getTestExportAnnSrcSpan loc =
      case parseAutoCollectAnn loc of
        Just AutoCollectTestExport -> Just (getLoc loc)
        _ -> Nothing
    exportIE = IEVar NoExtField $ generatedLoc $ IEName $ generatedLoc $ mkRdrUnqual testsIdentifier

-- | Identifier for the generated `tests` list.
testsIdentifier :: OccName
testsIdentifier = mkOccName NameSpace.varName "tasty_autocollect_tests"

{----- GenLocated utilities -----}

generatedLoc :: e -> Located e
generatedLoc = L generatedSrcSpan

firstLocatedWhere :: Ord l => (GenLocated l e -> Maybe a) -> [GenLocated l e] -> Maybe a
firstLocatedWhere f = listToMaybe . mapMaybe f . sortOn getLoc
