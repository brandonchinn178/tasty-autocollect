{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.ConvertTest (
  plugin,
) where

import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.ExternalNames
import Test.Tasty.AutoCollect.GHC

-- | The plugin to convert a test file. Injected by the preprocessor.
plugin :: Plugin
plugin =
  setKeepRawTokenStream
    defaultPlugin
      { pluginRecompile = purePlugin
      , parsedResultAction = \_ _ modl -> do
          env <- getHscEnv
          names <- liftIO $ loadExternalNames env
          pure $ transformTestModule names modl
      }

{- |
Transforms a test module of the form

@
{\- AUTOCOLLECT.TEST -\}
module MyTest (
  foo,
  {\- AUTOCOLLECT.TEST.export -\}
  bar,
) where

test = ...
@

to the equivalent of

@
module MyTest (
  foo,
  tasty_tests,
  bar,
) where

tasty_tests :: [TestTree]
tasty_tests = [tasty_test_1]

tasty_test_1 :: TestTree
tasty_test_1 = ...
@
-}
transformTestModule :: ExternalNames -> HsParsedModule -> HsParsedModule
transformTestModule names parsedModl = parsedModl{hpm_module = updateModule <$> hpm_module parsedModl}
  where
    updateModule modl =
      let (decls, testNames) = runConvertTestM $ concatMapM (convertTest names) $ hsmodDecls modl
       in modl
            { hsmodExports = updateExports <$> hsmodExports modl
            , hsmodDecls = mkTestsList testNames ++ decls
            }

    -- Replace "{- AUTOCOLLECT.TEST.export -}" with `tests` in the export list
    updateExports lexports
      | Just exportSpan <- firstLocatedWhere getTestExportAnnSrcSpan (getExportComments parsedModl lexports) =
          (L (toSrcAnnA exportSpan) exportIE :) <$> lexports
      | otherwise =
          lexports
    getTestExportAnnSrcSpan (L loc comment) =
      if isTestExportComment comment
        then Just loc
        else Nothing
    exportIE = IEVar NoExtField $ genLoc $ IEName testListName

    -- Generate the `tests` list
    mkTestsList :: [LocatedN RdrName] -> [LHsDecl GhcPs]
    mkTestsList testNames =
      let testsList = genLoc $ mkExplicitList $ map lhsvar testNames
       in [ genLoc $ genFuncSig testListName $ getListOfTestTreeType names
          , genLoc $ genFuncDecl testListName [] (flattenTestList testsList) Nothing
          ]

    flattenTestList testsList =
      mkHsApp (lhsvar $ genLoc $ getRdrName $ name_concat names) $
        mkExprTypeSig testsList . genLoc $
          HsListTy noAnn (getListOfTestTreeType names)

{- |
If the given declaration is a test, return the converted test, or otherwise
return it unmodified
-}
convertTest :: ExternalNames -> LHsDecl GhcPs -> ConvertTestM [LHsDecl GhcPs]
convertTest names ldecl =
  case parseDecl ldecl of
    Just (FuncSig [funcName] ty)
      | Just testType <- parseTestType (fromRdrName funcName) -> do
          testName <- getNextTestName
          setLastSeenSig
            SigInfo
              { testType
              , testName
              , signatureType = ty
              }
          unless (isValidForTestType names testType ty) $
            autocollectError . unlines $
              [ "Expected type: " ++ typeForTestType testType
              , "Got: " ++ showPpr ty
              ]
          pure [genFuncSig testName (getListOfTestTreeType names) <$ ldecl]
    Just (FuncDef funcName funcDefs)
      | Just testType <- parseTestType (fromRdrName funcName) -> do
          mSigInfo <- getLastSeenSig
          concatMapM (convertSingleTest funcName testType mSigInfo . unLoc) funcDefs
    -- anything else leave unmodified
    _ -> pure [ldecl]
  where
    convertSingleTest funcName testType mSigInfo FuncSingleDef{..} = do
      (testName, _, needsFuncSig) <-
        case mSigInfo of
          Nothing -> do
            testName <- getNextTestName
            pure (testName, Nothing, True)
          Just SigInfo{testType = testTypeFromSig, ..}
            | testType == testTypeFromSig -> pure (testName, Just signatureType, False)
            | otherwise -> autocollectError $ "Found test with different type of signature: " ++ show (testType, testTypeFromSig)

      funcBody <-
        case funcDefGuards of
          [FuncGuardedBody [] body] -> pure body
          _ ->
            autocollectError . unlines $
              [ "Test should have no guards."
              , "Found guards at " ++ getSpanLine funcName
              ]

      testBody <-
        case testType of
          TestNormal -> do
            checkNoArgs testType funcDefArgs
            pure $ singleExpr funcBody
          TestTodo -> do
            checkNoArgs testType funcDefArgs
            pure . singleExpr $
              mkHsApp
                (lhsvar $ genLoc $ getRdrName $ name_testTreeTodo names)
                (mkExprTypeSig funcBody $ mkHsTyVar (name_String names))
          TestBatch -> do
            checkNoArgs testType funcDefArgs
            pure funcBody

      pure . concat $
        [ if needsFuncSig
            then [genLoc $ genFuncSig testName (getListOfTestTreeType names)]
            else []
        , [genFuncDecl testName [] testBody (Just funcDefWhereClause) <$ ldecl]
        ]

    singleExpr = genLoc . mkExplicitList . (: [])

    checkNoArgs testType args =
      unless (null args) $
        autocollectError . unwords $
          [ showTestType testType ++ " should not be used with arguments"
          , "(at " ++ getSpanLine ldecl ++ ")"
          ]

-- | Identifier for the generated `tests` list.
testListName :: LocatedN RdrName
testListName = mkLRdrName testListIdentifier

data TestType
  = TestNormal
  | TestTodo
  | TestBatch
  deriving (Show, Eq)

parseTestType :: String -> Maybe TestType
parseTestType = \case
  "test" -> Just TestNormal
  "test_todo" -> Just TestTodo
  "test_batch" -> Just TestBatch
  _ -> Nothing

showTestType :: TestType -> String
showTestType = \case
  TestNormal -> "test"
  TestTodo -> "test_todo"
  TestBatch -> "test_batch"

isValidForTestType :: ExternalNames -> TestType -> LHsSigWcType GhcPs -> Bool
isValidForTestType names = \case
  TestNormal -> parsedTypeMatches $ isTypeVarNamed (name_TestTree names)
  TestTodo -> parsedTypeMatches $ isTypeVarNamed (name_String names)
  TestBatch -> parsedTypeMatches $ \case
    TypeList ty -> isTypeVarNamed (name_TestTree names) ty
    _ -> False
  where
    parsedTypeMatches f = maybe False f . parseSigWcType

typeForTestType :: TestType -> String
typeForTestType = \case
  TestNormal -> "TestTree"
  TestTodo -> "String"
  TestBatch -> "[TestTree]"

isTypeVarNamed :: Name -> ParsedType -> Bool
isTypeVarNamed name = \case
  TypeVar _ (L _ n) -> rdrNameOcc n == rdrNameOcc (getRdrName name)
  _ -> False

-- | Return the `[TestTree]` type.
getListOfTestTreeType :: ExternalNames -> LHsType GhcPs
getListOfTestTreeType names = genLoc $ HsListTy noAnn $ mkHsTyVar (name_TestTree names)

{----- Test converter monad -----}

type ConvertTestM = State ConvertTestState

data ConvertTestState = ConvertTestState
  { lastSeenSig :: Maybe SigInfo
  , allTests :: Seq (LocatedN RdrName)
  }

data SigInfo = SigInfo
  { testType :: TestType
  -- ^ The type of test represented in this signature
  , testName :: LocatedN RdrName
  -- ^ The generated name for the test
  , signatureType :: LHsSigWcType GhcPs
  -- ^ The type captured in the signature
  }

runConvertTestM :: ConvertTestM a -> (a, [LocatedN RdrName])
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

getNextTestName :: ConvertTestM (LocatedN RdrName)
getNextTestName = do
  state@ConvertTestState{allTests} <- State.get
  let nextTestName = mkLRdrName $ testIdentifier (length allTests)
  State.put state{allTests = allTests Seq.|> nextTestName}
  pure nextTestName

{----- Utilities -----}

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
