{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.ConvertTest (
  plugin,
) where

import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Foldable (toList)
import Data.List (intercalate, stripPrefix)
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

test_<tester> :: <type>
test_<tester> <name> <other args> = <test>
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
test1 = <tester> <name> <other args> (<test> :: <type>)
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
      mkHsApp (lhsvar $ mkLRdrName "concat") $
        genLoc . ExprWithTySig noAnn testsList $
          HsWC NoExtField . hsTypeToHsSigType . genLoc $
            HsListTy noAnn (getListOfTestTreeType names)

{- |
If the given declaration is a test, return the converted test, or otherwise
return it unmodified
-}
convertTest :: ExternalNames -> LHsDecl GhcPs -> ConvertTestM [LHsDecl GhcPs]
convertTest names ldecl =
  case parseDecl ldecl of
    -- e.g. test_testCase :: Assertion
    -- =>   test1 :: [TestTree]
    Just (FuncSig [funcName] ty)
      | Just testType <- parseTestType funcName -> do
          testName <- getNextTestName
          setLastSeenSig
            SigInfo
              { testType
              , testName
              , testHsType = ty
              }
          pure [genFuncSig testName (getListOfTestTreeType names) <$ ldecl]
    -- e.g. test_testCase "test name" = <body>
    -- =>   test1 = [testCase "test name" (<body> :: Assertion)]
    Just (FuncDef funcName funcDefs)
      | Just testType <- parseTestType funcName -> do
          mSigInfo <- getLastSeenSig

            case funcDefs of
              [] -> autocollectError $ "Test unexpectedly had no bindings at " ++ getSpanLine funcName
              [funcDef] -> convertSingleTest funcName testType mSigInfo (unLoc funcDef)
              _ ->
                autocollectError . unlines $
                  [ "Found multiple tests named " ++ fromRdrName funcName ++ " at: " ++ intercalate ", " (map getSpanLine funcDefs)
                  , "Did you forget to add a type annotation for a test?"
                  ]
    -- anything else leave unmodified
    _ -> pure [ldecl]
  where
    convertSingleTest funcName testType mSigInfo FuncSingleDef{..} = do
          (testName, funcBodyType) <-
            case mSigInfo of
              Nothing -> autocollectError $ "Found test without type signature at " ++ getSpanLine funcName
              Just SigInfo{testType = testTypeFromSig, ..}
                | testType == testTypeFromSig -> pure (testName, testHsType)
                | otherwise -> autocollectError $ "Found test with different type of signature: " ++ show (testType, testTypeFromSig)

          funcBody <-
            case funcDefGuards of
              [FuncGuardedBody [] body] -> pure body
              _ ->
                autocollectError . unlines $
                  [ "Test should have no guards."
                  , "Found guards at " ++ getSpanLine funcName
                  ]

          -- tester (...funcArgs) (funcBody :: funcBodyType)
          let funcBodyWithType = genLoc $ ExprWithTySig noAnn funcBody funcBodyType
              testBody =
                case testType of
                  TestSingle tester ->
                    genLoc . mkExplicitList $
                      [ mkHsApps (lhsvar $ genLoc $ fromTester names tester) $
                          map patternToExpr funcDefArgs ++ [funcBodyWithType]
                      ]
                  TestBatch
                    | not (null funcDefArgs) -> autocollectError "test_batch should not be used with arguments"
                    | not (isListOfTestTree names funcBodyType) -> autocollectError "test_batch needs to be set to a [TestTree]"
                    | otherwise -> funcBodyWithType

          pure [genFuncDecl testName [] testBody (Just funcDefWhereClause) <$ ldecl]

{- |
Convert the given pattern to the expression that it would represent
if it were in an expression context.
-}
patternToExpr :: LPat GhcPs -> LHsExpr GhcPs
patternToExpr lpat = go (parsePat lpat)
  where
    unsupported label = autocollectError $ label ++ " unsupported as test argument at " ++ getSpanLine lpat
    go = \case
      PatWildCard -> unsupported "wildcard patterns"
      PatVar name -> genLoc $ HsVar NoExtField name
      PatLazy -> unsupported "lazy patterns"
      PatAs -> unsupported "as patterns"
      PatParens p -> genLoc $ HsPar noAnn $ go p
      PatBang -> unsupported "bang patterns"
      PatList ps -> genLoc $ mkExplicitList $ map go ps
      PatTuple ps boxity -> genLoc $ mkExplicitTuple (map (Present noAnn . go) ps) boxity
      PatSum -> unsupported "anonymous sum patterns"
      PatConstructor name details ->
        case details of
          ConstructorPrefix tys args -> lhsvar name `mkHsAppTypes` tys `mkHsApps` map go args
          ConstructorRecord HsRecFields{..} ->
            genLoc . RecordCon noAnn name $
              HsRecFields
                { rec_flds = (fmap . fmap . fmap) go rec_flds
                , ..
                }
          ConstructorInfix l r -> mkHsApps (lhsvar name) $ map go [l, r]
      PatView -> unsupported "view patterns"
      PatSplice splice -> genLoc $ HsSpliceE noAnn splice
      PatLiteral lit -> genLoc $ HsLit noAnn lit
      PatOverloadedLit lit -> genLoc $ HsOverLit noAnn (unLoc lit)
      PatNPlusK -> unsupported "n+k patterns"
      PatTypeSig p ty -> genLoc $ ExprWithTySig noAnn (go p) $ hsTypeToHsSigWcType (genLoc (unLoc ty))

-- | Identifier for the generated `tests` list.
testListName :: LocatedN RdrName
testListName = mkLRdrName testListIdentifier

data TestType
  = TestSingle Tester
  | TestBatch
  deriving (Show, Eq)

data Tester
  = Tester String
  | TesterTodo
  deriving (Show, Eq)

parseTestType :: LocatedN RdrName -> Maybe TestType
parseTestType = fmap toTestType . stripPrefix "test_" . fromRdrName
  where
    toTestType = \case
      "batch" -> TestBatch
      "todo" -> TestSingle TesterTodo
      s -> TestSingle (Tester s)

fromTester :: ExternalNames -> Tester -> RdrName
fromTester names = \case
  Tester name -> mkRdrName name
  TesterTodo -> getRdrName $ name_testTreeTodo names

-- | Return the `[TestTree]` type.
getListOfTestTreeType :: ExternalNames -> LHsType GhcPs
getListOfTestTreeType names =
  (genLoc . HsListTy noAnn)
    . (genLoc . HsTyVar noAnn NotPromoted)
    $ genLoc testTreeName
  where
    testTreeName = getRdrName (name_TestTree names)

-- | Return True if the given type is `[TestTree]`.
isListOfTestTree :: ExternalNames -> LHsSigWcType GhcPs -> Bool
isListOfTestTree names ty =
  case parseSigWcType ty of
    Just (TypeList (TypeVar _ (L _ name))) -> rdrNameOcc name == rdrNameOcc testTreeName
    _ -> False
  where
    testTreeName = getRdrName (name_TestTree names)

{----- Test converter monad -----}

type ConvertTestM = State ConvertTestState

data ConvertTestState = ConvertTestState
  { lastSeenSig :: Maybe SigInfo
  , allTests :: Seq (LocatedN RdrName)
  }

data SigInfo = SigInfo
  { testType :: TestType
  -- ^ The parsed tester
  , testName :: LocatedN RdrName
  -- ^ The generated name for the test
  , testHsType :: LHsSigWcType GhcPs
  -- ^ The type of the test body
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
