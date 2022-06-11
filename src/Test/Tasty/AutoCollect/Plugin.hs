{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Char (toLower)
import Data.Either (lefts)
import Data.Foldable (toList)
import Data.List (sortOn, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Hs (
  GhcPs,
  HsBindLR (..),
  HsDecl (..),
  HsImplicitBndrs (..),
  HsModule (..),
  HsType (..),
  HsWildCardBndrs (..),
  IE (..),
  IEWrappedName (..),
  LHsDecl,
  LHsSigWcType,
  NoExtField (..),
  Sig (..),
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

test_testCase :: <type>
test_testCase <name> <other args> = <test>
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
      let (decls, testNames) = runConvertTestM $ mapM convertTest $ hsmodDecls modl
       in modl
            { hsmodExports = updateExports <$> hsmodExports modl
            , hsmodDecls = mkTestsList testNames : decls
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
    exportIE = IEVar NoExtField $ generatedLoc $ IEName testListIdentifier

    -- If the given declaration is a test, return Left with the converted test, or otherwise
    -- Right with the unmodified declaration
    convertTest :: LHsDecl GhcPs -> ConvertTestM (LHsDecl GhcPs)
    convertTest loc =
      case unLoc loc of
        SigD extS (TypeSig extT [funcName] ty)
          | Just tester <- getTester funcName -> do
              testName <- getNextTestName
              setLastSeenSig
                SigInfo
                  { testerName = tester
                  , testName
                  , testType = ty
                  }
              let testTreeType = HsWC NoExtField $ HsIB NoExtField $ generatedLoc $ HsTyVar NoExtField NotPromoted $ generatedLoc tastyTestTreeName
              let newSig = SigD extS $ TypeSig extT [testName] testTreeType
              pure (newSig <$ loc)
        ValD extV (FunBind extF funcName _ _)
          | Just tester <- getTester funcName -> do
              (testName, mType) <- getLastSeenSig >>= \case
                Nothing -> do
                  testName <- getNextTestName
                  pure (testName, Nothing)
                Just SigInfo{..} -> pure (testName, Just testType)
              -- TODO: error if MatchGroup is not exactly one item (only support one func clause)
              -- TODO: convert match [tester ...args = body] into expression [tester ...args body]
              -- TODO: if lastSeenSig has type, set type of [body]
              let testBody = _
              let newFunc = ValD extV (FunBind extF testName testBody [])
              pure (newFunc <$ loc)
        _ -> pure loc

    -- Generate the `tests` list
    mkTestsList :: [Located RdrName] -> LHsDecl GhcPs
    mkTestsList _ =
      let testsList = _ -- TODO
       in generatedLoc $ ValD NoExtField $ FunBind NoExtField testListIdentifier testsList []

-- | Identifier for the generated `tests` list.
testListIdentifier :: Located RdrName
testListIdentifier = mkRdrName "tasty_autocollect_tests"

getTester :: Located RdrName -> Maybe String
getTester = stripPrefix "test_" . fromRdrName

-- | RdrName for 'Test.Tasty.TestTree'
tastyTestTreeName :: RdrName
tastyTestTreeName = mkRdrQual (mkModuleName "Test.Tasty") (mkOccName NameSpace.varName "TestTree")

{----- Test converter monad -----}

type ConvertTestM = State ConvertTestState

data ConvertTestState = ConvertTestState
  { lastSeenSig :: Maybe SigInfo
  , allTests :: Seq (Located RdrName)
  }

data SigInfo = SigInfo
  { testerName :: String
  -- ^ The name of the tester (e.g. "testCase")
  , testName :: Located RdrName
  -- ^ The generated name for the test
  , testType :: LHsSigWcType GhcPs
  -- ^ The type of the test body
  }

runConvertTestM :: ConvertTestM a -> (a, [Located RdrName])
runConvertTestM m =
  fmap (toList . allTests) . State.runState m $
    ConvertTestState
      { lastSeenSig = Nothing
      , allTests = Seq.Empty
      }

getLastSeenSig :: ConvertTestM (Maybe SigInfo)
getLastSeenSig = do
  state@ConvertTestState{lastSeenSig} <- State.get
  State.put state{lastSeenSig = Nothing}
  pure lastSeenSig

setLastSeenSig :: SigInfo -> ConvertTestM ()
setLastSeenSig info = State.modify' $ \state -> state{lastSeenSig = Just info}

getNextTestName :: ConvertTestM (Located RdrName)
getNextTestName = do
  state@ConvertTestState{allTests} <- State.get
  let nextTestName = mkRdrName $ "tasty_autocollect_test_" ++ show (length allTests)
  State.put state{allTests = allTests Seq.|> nextTestName}
  pure nextTestName


{----- GenLocated utilities -----}

generatedLoc :: e -> Located e
generatedLoc = L generatedSrcSpan

firstLocatedWhere :: Ord l => (GenLocated l e -> Maybe a) -> [GenLocated l e] -> Maybe a
firstLocatedWhere f = listToMaybe . mapMaybe f . sortOn getLoc

{----- Name utilities -----}

mkRdrName :: String -> Located RdrName
mkRdrName = generatedLoc . mkRdrUnqual . mkOccName NameSpace.varName

fromRdrName :: Located RdrName -> String
fromRdrName = occNameString . rdrNameOcc . unLoc
