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
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , pluginRecompile = purePlugin
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
    getCommentsAt = getAnnotationComments (hpm_annotations parsedModl)

    updateModule modl =
      let (decls, testNames) = runConvertTestM $ mapM (convertTest names) $ hsmodDecls modl
       in modl
            { hsmodExports = updateExports <$> hsmodExports modl
            , hsmodDecls = mkTestsList testNames ++ decls
            }

    -- Replace "{- AUTOCOLLECT.TEST.export -}" with `tests` in the export list
    updateExports loc
      | RealSrcSpan realSrcSpan _ <- getLoc loc
      , Just exportSpan <- firstLocatedWhere getTestExportAnnSrcSpan (getCommentsAt realSrcSpan) =
          (L (RealSrcSpan exportSpan Nothing) exportIE :) <$> loc
      | otherwise =
          loc
    getTestExportAnnSrcSpan loc =
      if isTestExportComment (getCommentContent loc)
        then Just (getLoc loc)
        else Nothing
    exportIE = IEVar NoExtField $ genLoc $ IEName testListName

    -- Generate the `tests` list
    mkTestsList :: [Located RdrName] -> [LHsDecl GhcPs]
    mkTestsList testNames =
      let testsList = genList $ map lhsvar testNames
       in [ genLoc $ genFuncSig testListName $ getListOfTestTreeType names
          , genLoc $ genFuncDecl testListName [] (flattenTestList testsList) Nothing
          ]

    flattenTestList testsList =
      mkHsApp (lhsvar $ mkLRdrName "concat") $
        genLoc . ExprWithTySig NoExtField testsList $
          HsWC NoExtField . HsIB NoExtField . genLoc $
            HsListTy NoExtField (getListOfTestTreeType names)

{- |
If the given declaration is a test, return the converted test, or otherwise
return it unmodified
-}
convertTest :: ExternalNames -> LHsDecl GhcPs -> ConvertTestM (LHsDecl GhcPs)
convertTest names loc =
  case unLoc loc of
    -- e.g. test_testCase :: Assertion
    -- =>   test1 :: [TestTree]
    SigD _ (TypeSig _ [funcName] ty)
      | Just testType <- parseTestType funcName -> do
          testName <- getNextTestName
          setLastSeenSig
            SigInfo
              { testType
              , testName
              , testHsType = ty
              }
          pure (genFuncSig testName (getListOfTestTreeType names) <$ loc)
    -- e.g. test_testCase "test name" = <body>
    -- =>   test1 = [testCase "test name" (<body> :: Assertion)]
    ValD _ (FunBind _ funcName funcMatchGroup _)
      | Just testType <- parseTestType funcName -> do
          (testName, funcBodyType) <-
            getLastSeenSig >>= \case
              Nothing -> autocollectError $ "Found test without type signature at " ++ getSpanLine funcName
              Just SigInfo{testType = testTypeFromSig, ..}
                | testType == testTypeFromSig -> pure (testName, testHsType)
                | otherwise -> autocollectError $ "Found test with different type of signature: " ++ show (testType, testTypeFromSig)

          let MG{mg_alts = L _ funcMatches} = funcMatchGroup
          funcMatch <-
            case funcMatches of
              [] -> autocollectError $ "Test unexpectedly had no bindings at " ++ getSpanLine funcName
              [L _ funcMatch] -> pure funcMatch
              _ ->
                autocollectError . unlines $
                  [ "Found multiple tests named " ++ fromRdrName funcName ++ " at: " ++ intercalate ", " (map getSpanLine funcMatches)
                  , "Did you forget to add a type annotation for a test?"
                  ]

          let Match{m_pats = funcArgs, m_grhss = GRHSs{grhssGRHSs, grhssLocalBinds = whereClause}} = funcMatch
          funcBody <-
            case grhssGRHSs of
              [L _ (GRHS _ [] funcBody)] -> pure funcBody
              _ ->
                autocollectError . unlines $
                  [ "Test should have no guards."
                  , "Found guards at " ++ getSpanLine funcName
                  ]

          -- tester (...funcArgs) (funcBody :: funcBodyType)
          let funcBodyWithType = genLoc $ ExprWithTySig NoExtField funcBody funcBodyType
              testBody =
                case testType of
                  TestSingle tester ->
                    genList
                      [ mkHsApps (lhsvar $ genLoc $ fromTester names tester) $
                          map patternToExpr funcArgs ++ [funcBodyWithType]
                      ]
                  TestBatch
                    | not (null funcArgs) -> autocollectError "test_batch should not be used with arguments"
                    | not (isListOfTestTree names funcBodyType) -> autocollectError "test_batch needs to be set to a [TestTree]"
                    | otherwise -> funcBodyWithType

          pure (genFuncDecl testName [] testBody (Just whereClause) <$ loc)
    -- anything else leave unmodified
    _ -> pure loc

{- |
Convert the given pattern to the expression that it would represent
if it were in an expression context.
-}
patternToExpr :: LPat GhcPs -> LHsExpr GhcPs
patternToExpr lpat =
  case unLoc lpat of
    WildPat{} -> unsupported "wildcard patterns"
    VarPat _ name -> genLoc $ HsVar NoExtField name
    LazyPat{} -> unsupported "lazy patterns"
    AsPat{} -> unsupported "as patterns"
    ParPat _ p -> genLoc $ HsPar NoExtField $ patternToExpr p
    BangPat{} -> unsupported "bang patterns"
    ListPat _ ps -> genList $ map patternToExpr ps
    TuplePat _ ps boxity -> genLoc $ ExplicitTuple NoExtField (map (genLoc . Present NoExtField . patternToExpr) ps) boxity
    SumPat{} -> unsupported "anonymous sum patterns"
    ConPat _ conName conDetails ->
      case conDetails of
        PrefixCon args -> mkHsApps (lhsvar conName) $ map patternToExpr args
        RecCon fields -> genLoc $ RecordCon NoExtField conName $ patternToExpr <$> fields
        InfixCon l r -> mkHsApps (lhsvar conName) $ map patternToExpr [l, r]
    ViewPat{} -> unsupported "view patterns"
    SplicePat _ splice -> genLoc $ HsSpliceE NoExtField splice
    LitPat _ lit -> genLoc $ HsLit NoExtField lit
    NPat _ lit _ _ -> genLoc $ HsOverLit NoExtField (unLoc lit)
    NPlusKPat{} -> unsupported "n+k patterns"
    SigPat _ p (HsPS _ ty) -> genLoc $ ExprWithTySig NoExtField (patternToExpr p) $ mkLHsSigWcType (genLoc (unLoc ty))
  where
    unsupported label = autocollectError $ label ++ " unsupported as test argument at " ++ getSpanLine lpat

-- | Identifier for the generated `tests` list.
testListName :: Located RdrName
testListName = mkLRdrName testListIdentifier

data TestType
  = TestSingle Tester
  | TestBatch
  deriving (Show, Eq)

data Tester
  = Tester String
  | TesterTodo
  deriving (Show, Eq)

parseTestType :: Located RdrName -> Maybe TestType
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
  (genLoc . HsListTy NoExtField)
    . (genLoc . HsTyVar NoExtField NotPromoted)
    $ genLoc testTreeName
  where
    testTreeName = getRdrName (name_TestTree names)

-- | Return True if the given type is `[TestTree]`.
isListOfTestTree :: ExternalNames -> LHsSigWcType GhcPs -> Bool
isListOfTestTree names ty =
  case hsib_body $ hswc_body ty of
    L _ (HsListTy _ (L _ (HsTyVar _ _ (L _ name)))) -> rdrNameOcc name == rdrNameOcc testTreeName
    _ -> False
  where
    testTreeName = getRdrName (name_TestTree names)

{----- Test converter monad -----}

type ConvertTestM = State ConvertTestState

data ConvertTestState = ConvertTestState
  { lastSeenSig :: Maybe SigInfo
  , allTests :: Seq (Located RdrName)
  }

data SigInfo = SigInfo
  { testType :: TestType
  -- ^ The parsed tester
  , testName :: Located RdrName
  -- ^ The generated name for the test
  , testHsType :: LHsSigWcType GhcPs
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
  let nextTestName = mkLRdrName $ testIdentifier (length allTests)
  State.put state{allTests = allTests Seq.|> nextTestName}
  pure nextTestName
