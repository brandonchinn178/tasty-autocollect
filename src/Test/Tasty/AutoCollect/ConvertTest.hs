{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Tasty.AutoCollect.ConvertTest (
  plugin,
) where

import Control.Arrow ((&&&))
import Control.Monad (unless, zipWithM)
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text

import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.ExternalNames
import Test.Tasty.AutoCollect.GHC hiding (comment)

-- | The plugin to convert a test file. Injected by the preprocessor.
plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = \_ env ->
        pure
          env
            { hsc_dflags = hsc_dflags env `gopt_set` Opt_KeepRawTokenStream
            }
    , pluginRecompile = purePlugin
    , parsedResultAction = \_ _ result -> do
        env <- getHscEnv
        names <- liftIO $ loadExternalNames env
        pure
          result
            { parsedResultModule = transformTestModule names $ parsedResultModule result
            }
    }

-- | Transforms a test module of the form
--
-- @
-- {\- AUTOCOLLECT.TEST -\}
-- module MyTest (
--   foo,
--   {\- AUTOCOLLECT.TEST.export -\}
--   bar,
-- ) where
--
-- test = ...
-- @
--
-- to the equivalent of
--
-- @
-- module MyTest (
--   foo,
--   tasty_tests,
--   bar,
-- ) where
--
-- tasty_tests :: [TestTree]
-- tasty_tests = [tasty_test_1]
--
-- tasty_test_1 :: TestTree
-- tasty_test_1 = ...
-- @
transformTestModule :: ExternalNames -> HsParsedModule -> HsParsedModule
transformTestModule names parsedModl = parsedModl{hpm_module = updateModule <$> hpm_module parsedModl}
  where
    updateModule modl =
      let (decls, testNames) = runConvertTestModuleM $ concatMapM (convertTest names) $ hsmodDecls modl
       in modl
            { hsmodExports = updateExports <$> hsmodExports modl
            , hsmodDecls = mkTestsList testNames ++ decls
            }

    -- Replace "{- AUTOCOLLECT.TEST.export -}" with `tests` in the export list
    updateExports lexports
      | Just exportSpan <- firstLocatedWhere getTestExportAnnSrcSpan (getExportComments lexports) =
          (L (toSrcAnnA exportSpan) exportIE :) <$> lexports
      | otherwise =
          lexports
    getTestExportAnnSrcSpan (L loc comment) =
      if isTestExportComment comment
        then Just loc
        else Nothing
    exportIE = mkIEVar $ genLoc $ mkIEName testListName

    -- Generate the `tests` list
    mkTestsList :: [LocatedN RdrName] -> [LHsDecl GhcPs]
    mkTestsList testNames =
      let testsList = genLoc $ ExplicitList noAnn $ map lhsvar testNames
       in [ genLoc $ genFuncSig testListName $ getListOfTestTreeType names
          , genLoc $ genFuncDecl testListName [] (flattenTestList testsList) Nothing
          ]

    flattenTestList testsList =
      mkHsApp (mkHsVar $ name_concat names) $
        mkExprTypeSig testsList . genLoc $
          HsListTy noAnn (getListOfTestTreeType names)

-- | If the given declaration is a test, return the converted test, or otherwise
-- return it unmodified
convertTest :: ExternalNames -> LHsDecl GhcPs -> ConvertTestModuleM [LHsDecl GhcPs]
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
          concat <$> zipWithM (convertSingleTest funcName testType) (mSigInfo : repeat Nothing) funcDefs
    -- anything else leave unmodified
    _ -> pure [ldecl]
  where
    loc = getLocA ldecl

    convertSingleTest funcName testType mSigInfo (L _ FuncSingleDef{..}) = do
      (testName, mSigType) <-
        case mSigInfo of
          Nothing -> do
            testName <- getNextTestName
            pure (testName, Nothing)
          Just SigInfo{testType = testTypeFromSig, ..}
            | testType == testTypeFromSig -> pure (testName, Just signatureType)
            | otherwise -> autocollectError $ "Found test with different type of signature: " ++ show (testType, testTypeFromSig)

      (testBody, ConvertTestState{mWhereClause}) <-
        case funcDefGuards of
          [FuncGuardedBody [] body] -> do
            let state =
                  ConvertTestState
                    { mSigType
                    , testArgs = funcDefArgs
                    , mWhereClause = Just funcDefWhereClause
                    }
            pure . runConvertTestM state $ do
              testBody <- convertSingleTestBody testType body
              State.gets testArgs >>= \case
                [] -> pure ()
                _ -> autocollectError $ "Found extraneous arguments at " ++ getSpanLine loc
              pure testBody
          _ ->
            autocollectError . unlines $
              [ "Test should have no guards."
              , "Found guards at " ++ getSpanLine (getLocA funcName)
              ]

      pure . concat $
        [ if isNothing mSigInfo
            then [genLoc $ genFuncSig testName (getListOfTestTreeType names)]
            else []
        , [genFuncDecl testName [] testBody mWhereClause <$ ldecl]
        ]

    convertSingleTestBody testType body =
      case testType of
        TestNormal ->
          pure $ singleExpr body
        TestProp -> do
          -- test_prop :: <type>
          -- test_prop "name" arg1 arg2 = <body> where <defs>
          -- ====>
          -- test = testProperty "name" ((\arg1 arg2 -> let <defs> in <body>) :: <type>)

          state@ConvertTestState{mSigType, mWhereClause} <- State.get
          State.put state{mSigType = Nothing, mWhereClause = Nothing}

          (name, remainingPats) <-
            popRemainingArgs >>= \case
              arg : rest | Just s <- parseLitStrPat arg -> pure (s, rest)
              [] -> autocollectError "test_prop requires at least the name of the test"
              arg : _ ->
                autocollectError . unlines $
                  [ "test_prop expected a String for the name of the test."
                  , "Got: " ++ showPpr arg
                  ]

          let propBody =
                mkHsLam remainingPats $
                  case mWhereClause of
                    Just defs -> genLoc $ mkLet defs body
                    Nothing -> body

          pure . singleExpr $
            mkHsApps
              (lhsvar $ mkLRdrName "testProperty")
              [ mkHsLitString name
              , maybe propBody (genLoc . ExprWithTySig noAnn propBody) mSigType
              ]
        TestTodo ->
          pure . singleExpr $
            mkHsApp
              (mkHsVar $ name_testTreeTodo names)
              (mkExprTypeSig body $ mkHsTyVar (name_String names))
        TestBatch ->
          pure body
        TestModify modifier testType' ->
          withTestModifier names modifier loc $
            convertSingleTestBody testType' body

    singleExpr = genLoc . ExplicitList noAnn . (: [])

-- | Identifier for the generated `tests` list.
testListName :: LocatedN RdrName
testListName = mkLRdrName testListIdentifier

-- | Return the `[TestTree]` type.
getListOfTestTreeType :: ExternalNames -> LHsType GhcPs
getListOfTestTreeType names = genLoc $ HsListTy noAnn $ mkHsTyVar (name_TestTree names)

{----- TestType -----}

data TestType
  = TestNormal
  | TestProp
  | TestTodo
  | TestBatch
  | TestModify TestModifier TestType
  deriving (Show, Eq)

data TestModifier
  = ExpectFail
  | ExpectFailBecause
  | IgnoreTest
  | IgnoreTestBecause
  deriving (Show, Eq)

parseTestType :: String -> Maybe TestType
parseTestType = go . Text.splitOn "_" . Text.pack
  where
    go = \case
      ["test"] -> Just TestNormal
      ["test", "prop"] -> Just TestProp
      ["test", "todo"] -> Just TestTodo
      ["test", "batch"] -> Just TestBatch
      (unsnoc -> Just (t, "expectFail")) -> TestModify ExpectFail <$> go t
      (unsnoc -> Just (t, "expectFailBecause")) -> TestModify ExpectFailBecause <$> go t
      (unsnoc -> Just (t, "ignoreTest")) -> TestModify IgnoreTest <$> go t
      (unsnoc -> Just (t, "ignoreTestBecause")) -> TestModify IgnoreTestBecause <$> go t
      _ -> Nothing

    unsnoc = fmap (NonEmpty.init &&& NonEmpty.last) . NonEmpty.nonEmpty

isValidForTestType :: ExternalNames -> TestType -> LHsSigWcType GhcPs -> Bool
isValidForTestType names = \case
  TestNormal -> parsedTypeMatches isTestTreeTypeVar
  TestProp -> const True
  TestTodo -> parsedTypeMatches $ isTypeVarNamed (name_String names)
  TestBatch -> parsedTypeMatches $ \case
    TypeList ty -> isTestTreeTypeVar ty
    _ -> False
  TestModify modifier tt -> isValidForModifier tt modifier
  where
    isValidForModifier tt = \case
      ExpectFail -> isValidForTestType names tt
      ExpectFailBecause -> isValidForTestType names tt
      IgnoreTest -> isValidForTestType names tt
      IgnoreTestBecause -> isValidForTestType names tt

    parsedTypeMatches f = maybe False f . parseSigWcType
    isTestTreeTypeVar = isTypeVarNamed (name_TestTree names)

typeForTestType :: TestType -> String
typeForTestType = \case
  TestNormal -> "TestTree"
  TestProp -> "(Testable prop => prop)"
  TestTodo -> "String"
  TestBatch -> "[TestTree]"
  TestModify modifier tt -> typeForTestModifier tt modifier
  where
    typeForTestModifier tt = \case
      ExpectFail -> typeForTestType tt
      ExpectFailBecause -> typeForTestType tt
      IgnoreTest -> typeForTestType tt
      IgnoreTestBecause -> typeForTestType tt

isTypeVarNamed :: Name -> ParsedType -> Bool
isTypeVarNamed name = \case
  TypeVar _ (L _ n) -> rdrNameOcc n == rdrNameOcc (getRdrName name)
  _ -> False

withTestModifier ::
  ExternalNames
  -> TestModifier
  -> SrcSpan
  -> ConvertTestM (LHsExpr GhcPs)
  -> ConvertTestM (LHsExpr GhcPs)
withTestModifier names modifier loc m =
  case modifier of
    ExpectFail -> mapAllTests (mkHsVar $ name_expectFail names) <$> m
    ExpectFailBecause ->
      popArg >>= \case
        Just arg
          | Just s <- parseLitStrPat arg ->
              mapAllTests (applyName (name_expectFailBecause names) [mkHsLitString s]) <$> m
        mArg -> needsStrArg mArg "_expectFailBecause"
    IgnoreTest -> mapAllTests (mkHsVar $ name_ignoreTest names) <$> m
    IgnoreTestBecause ->
      popArg >>= \case
        Just arg
          | Just s <- parseLitStrPat arg ->
              mapAllTests (applyName (name_ignoreTestBecause names) [mkHsLitString s]) <$> m
        mArg -> needsStrArg mArg "_ignoreTestBecause"
  where
    needsStrArg mArg label =
      autocollectError . unlines . concat $
        [ [label ++ " requires a String argument."]
        , case mArg of
            Nothing -> []
            Just arg -> ["Got: " ++ showPpr arg]
        , ["At: " ++ getSpanLine loc]
        ]

    applyName name = mkHsApps (mkHsVar name)

    -- mapAllTests f e = [| map $f $e |]
    mapAllTests func expr = applyName (name_map names) [func, expr]

{----- Test function converter monad -----}

type ConvertTestM = State ConvertTestState

data ConvertTestState = ConvertTestState
  { mSigType :: Maybe (LHsSigWcType GhcPs)
  , mWhereClause :: Maybe (HsLocalBinds GhcPs)
  , testArgs :: [LPat GhcPs]
  }

runConvertTestM :: ConvertTestState -> ConvertTestM a -> (a, ConvertTestState)
runConvertTestM = flip State.runState

popArg :: ConvertTestM (Maybe (LPat GhcPs))
popArg = do
  state <- State.get
  let (mArg, rest) =
        case testArgs state of
          [] -> (Nothing, [])
          arg : args -> (Just arg, args)
  State.put state{testArgs = rest}
  pure mArg

popRemainingArgs :: ConvertTestM [LPat GhcPs]
popRemainingArgs = do
  state@ConvertTestState{testArgs} <- State.get
  State.put state{testArgs = []}
  pure testArgs

{----- Test module converter monad -----}

type ConvertTestModuleM = State ConvertTestModuleState

data ConvertTestModuleState = ConvertTestModuleState
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

runConvertTestModuleM :: ConvertTestModuleM a -> (a, [LocatedN RdrName])
runConvertTestModuleM m =
  fmap (toList . allTests) . State.runState m $
    ConvertTestModuleState
      { lastSeenSig = Nothing
      , allTests = Seq.Empty
      }

getLastSeenSig :: ConvertTestModuleM (Maybe SigInfo)
getLastSeenSig = do
  state@ConvertTestModuleState{lastSeenSig} <- State.get
  State.put state{lastSeenSig = Nothing}
  pure lastSeenSig

setLastSeenSig :: SigInfo -> ConvertTestModuleM ()
setLastSeenSig info = State.modify' $ \state -> state{lastSeenSig = Just info}

getNextTestName :: ConvertTestModuleM (LocatedN RdrName)
getNextTestName = do
  state@ConvertTestModuleState{allTests} <- State.get
  let nextTestName = mkLRdrName $ testIdentifier (length allTests)
  State.put state{allTests = allTests Seq.|> nextTestName}
  pure nextTestName

{----- Utilities -----}

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
