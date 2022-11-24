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
import qualified Control.Monad.Trans.State.Strict as State
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

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
      , parsedResultAction = \_ _ result -> do
          env <- getHscEnv
          names <- liftIO $ loadExternalNames env
          pure $ withParsedResultModule result (transformTestModule names)
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
      let decls = hsmodDecls modl
          allResources = findAllResources decls
          convertTestEnv =
            ConvertTestEnv
              { externalNames = names
              , allResources = Map.fromList [(resourceName r, r) | r <- allResources]
              }
          (decls', tests) =
            runConvertTestM convertTestEnv $
              concatMapM convertTest decls
       in modl
            { hsmodExports = updateExports <$> hsmodExports modl
            , hsmodDecls = mkTestsList allResources tests ++ decls'
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
    mkTestsList :: [ResourceInfo] -> [TestInfo] -> [LHsDecl GhcPs]
    mkTestsList allResources tests =
      let testsList = genLoc $ mkExplicitList $ map mkTest tests
          testsBody = addResources allResources $ flattenTestList testsList
       in [ genLoc $ genFuncSig testListName $ getListOfTestTreeType names
          , genLoc $ genFuncDecl testListName [] testsBody Nothing
          ]
    flattenTestList testsList =
      mkHsApp (mkHsVar $ name_concat names) $
        mkExprTypeSig testsList . genLoc $
          HsListTy noAnn (getListOfTestTreeType names)

    -- Generate each test in the `tests` list
    mkTest :: TestInfo -> LHsExpr GhcPs
    mkTest TestInfo{..} =
      mkHsApps (lhsvar testInfoName) $
        [ lhsvar resourceRdrName
        | ResourceInfo{..} <- testInfoResources
        ]

{- |
If the given declaration is a test, return the converted test, or otherwise
return it unmodified
-}
convertTest :: LHsDecl GhcPs -> ConvertTestM [LHsDecl GhcPs]
convertTest ldecl = do
  names <- getExternalNames
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
      names <- getExternalNames
      (testName, mSigType) <-
        case mSigInfo of
          Nothing -> do
            testName <- getNextTestName
            pure (testName, Nothing)
          Just SigInfo{testType = testTypeFromSig, ..}
            | testType == testTypeFromSig -> pure (testName, Just signatureType)
            | otherwise -> autocollectError $ "Found test with different type of signature: " ++ show (testType, testTypeFromSig)

      (testBody, resources) <-
        case funcDefGuards of
          [FuncGuardedBody [] body] -> convertSingleTestBody testType mSigType funcDefArgs body
          _ ->
            autocollectError . unlines $
              [ "Test should have no guards."
              , "Found guards at " ++ getSpanLine (getLocA funcName)
              ]

      addTestInfo
        TestInfo
          { testInfoName = testName
          , testInfoResources = resources
          }

      let testArgs =
            [ mkVarPat resourceRdrName
            | ResourceInfo{..} <- resources
            ]

      pure . concat $
        [ if isNothing mSigInfo
            then [genLoc $ genFuncSig testName (getListOfTestTreeType names)]
            else []
        , [genFuncDecl testName testArgs testBody (Just funcDefWhereClause) <$ ldecl]
        ]

    convertSingleTestBody testType mSigType args body = do
      names <- getExternalNames
      case testType of
        TestNormal -> do
          (resources, args') <- extractResourceArgs args
          checkNoArgs testType args'
          pure (singleExpr body, resources)
        TestProp -> do
          (name, resources, remainingPats) <-
            case args of
              arg : rest | Just (PatLitString name) <- parsePat arg -> do
                (resources, remainingPats) <- extractResourceArgs rest
                pure (name, resources, remainingPats)
              [] -> autocollectError "test_prop requires at least the name of the test"
              arg : _ ->
                autocollectError . unlines $
                  [ "test_prop expected a String for the name of the test."
                  , "Got: " ++ showPpr arg
                  ]
          let propBody = mkHsLam remainingPats body
          pure
            ( singleExpr $
                mkHsApps
                  (lhsvar $ mkLRdrName "testProperty")
                  [ mkHsLitString name
                  , maybe propBody (genLoc . ExprWithTySig noAnn propBody) mSigType
                  ]
            , resources
            )
        TestTodo -> do
          (resources, args') <- extractResourceArgs args
          checkNoArgs testType args'
          pure
            ( singleExpr $
                mkHsApp
                  (mkHsVar $ name_testTreeTodo names)
                  (mkExprTypeSig body $ mkHsTyVar (name_String names))
            , resources
            )
        TestBatch -> do
          (resources, args') <- extractResourceArgs args
          checkNoArgs testType args'
          pure (body, resources)
        TestModify modifier testType' ->
          withTestModifier names modifier loc args $ \args' ->
            convertSingleTestBody testType' mSigType args' body

    singleExpr = genLoc . mkExplicitList . (: [])

    checkNoArgs testType args =
      unless (null args) $
        autocollectError . unwords $
          [ showTestType testType ++ " should not be used with arguments"
          , "(at " ++ getSpanLine loc ++ ")"
          ]

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

showTestType :: TestType -> String
showTestType = \case
  TestNormal -> "test"
  TestProp -> "test_prop"
  TestTodo -> "test_todo"
  TestBatch -> "test_batch"
  TestModify modifier tt -> showTestType tt ++ showModifier modifier
  where
    showModifier = \case
      ExpectFail -> "_expectFail"
      ExpectFailBecause -> "_expectFailBecause"
      IgnoreTest -> "_ignoreTest"
      IgnoreTestBecause -> "_ignoreTestBecause"

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
  Monad m =>
  ExternalNames ->
  TestModifier ->
  SrcSpan ->
  [LPat GhcPs] ->
  ([LPat GhcPs] -> m (LHsExpr GhcPs, a)) ->
  m (LHsExpr GhcPs, a)
withTestModifier names modifier loc args f =
  case modifier of
    ExpectFail -> mapAllTests (mkHsVar $ name_expectFail names) <$> f args
    ExpectFailBecause ->
      case args of
        arg : rest
          | Just (PatLitString s) <- parsePat arg ->
              mapAllTests (applyName (name_expectFailBecause names) [mkHsLitString s]) <$> f rest
        _ -> needsStrArg "_expectFailBecause"
    IgnoreTest -> mapAllTests (mkHsVar $ name_ignoreTest names) <$> f args
    IgnoreTestBecause ->
      case args of
        arg : rest
          | Just (PatLitString s) <- parsePat arg ->
              mapAllTests (applyName (name_ignoreTestBecause names) [mkHsLitString s]) <$> f rest
        _ -> needsStrArg "_ignoreTestBecause"
  where
    needsStrArg label =
      autocollectError . unlines . concat $
        [ [label ++ " requires a String argument."]
        , case args of
            [] -> []
            arg : _ -> ["Got: " ++ showPpr arg]
        , ["At: " ++ getSpanLine loc]
        ]

    applyName name = mkHsApps (mkHsVar name)

    -- mapAllTests f e = [| map $f $e |]
    mapAllTests func (expr, a) = (applyName (name_map names) [func, expr], a)

{----- Resources -----}

data ResourceInfo = ResourceInfo
  { resourceFunc :: LocatedN RdrName
  -- ^ The original `resource_` function name
  , resourceName :: Text
  -- ^ The parsed name from `resourceFunc`
  , resourceRdrName :: LocatedN RdrName
  -- ^ The RdrName corresponding to resourceName
  }

findAllResources :: [LHsDecl GhcPs] -> [ResourceInfo]
findAllResources = mapMaybe parseResourceInfo
  where
    parseResourceName = Text.stripPrefix "resource_" . Text.pack . fromRdrName

    parseResourceInfo decl =
      case parseDecl decl of
        Just (FuncDef name defs) | Just resourceName <- parseResourceName name ->
          checkResourceFunction name defs $
            Just
              ResourceInfo
                { resourceFunc = name
                , resourceName = resourceName
                , resourceRdrName = mkLRdrName $ Text.unpack resourceName
                }
        _ -> Nothing

    checkResourceFunction name defs x =
      case defs of
        [L _ FuncSingleDef{..}] | [FuncGuardedBody [] _] <- funcDefGuards -> x
        _ ->
          autocollectError . unlines $
            [ "Invalid resource definition: " ++ showPpr name
            , "`resource_` definitions must have exactly one function binding and no guards"
            ]

addResources :: [ResourceInfo] -> LHsExpr GhcPs -> LHsExpr GhcPs
addResources = flip (foldr go)
  where
    go ResourceInfo{..} testsList =
      -- [| resource_foo $ \resourceRdrName -> testsList |]
      mkHsApp (lhsvar resourceFunc) $
        mkHsLam [mkVarPat resourceRdrName] testsList

extractResourceArgs' :: Map Text ResourceInfo -> [LPat GhcPs] -> ([ResourceInfo], [LPat GhcPs])
extractResourceArgs' allResources = partitionEithers . map parseResourceArg
  where
    parseResourceArg pat =
      case parsePat pat of
        Just (PatPrefixCon name [PatVar arg]) | fromRdrName name == "Resource" ->
          case Text.pack (fromRdrName arg) `Map.lookup` allResources of
            Nothing -> autocollectError $ "Unknown resource: " ++ showPpr name
            Just resource -> Left resource
        _ -> Right pat

{----- Test converter monad -----}

type ConvertTestM = State ConvertTestState

data ConvertTestEnv = ConvertTestEnv
  { externalNames :: ExternalNames
  , allResources :: Map Text ResourceInfo
  }

data ConvertTestState = ConvertTestState
  { convertTestEnv :: ConvertTestEnv
  , lastSeenSig :: Maybe SigInfo
  , allTests :: Seq TestInfo
  }

data TestInfo = TestInfo
  { testInfoName :: LocatedN RdrName
  , testInfoResources :: [ResourceInfo]
  }

data SigInfo = SigInfo
  { testType :: TestType
  -- ^ The type of test represented in this signature
  , testName :: LocatedN RdrName
  -- ^ The generated name for the test
  , signatureType :: LHsSigWcType GhcPs
  -- ^ The type captured in the signature
  }

runConvertTestM :: ConvertTestEnv -> ConvertTestM a -> (a, [TestInfo])
runConvertTestM env m =
  fmap (toList . allTests) . State.runState m $
    ConvertTestState
      { convertTestEnv = env
      , lastSeenSig = Nothing
      , allTests = Seq.Empty
      }

getExternalNames :: ConvertTestM ExternalNames
getExternalNames = State.gets (externalNames . convertTestEnv)

getLastSeenSig :: ConvertTestM (Maybe SigInfo)
getLastSeenSig = do
  state@ConvertTestState{lastSeenSig} <- State.get
  State.put state{lastSeenSig = Nothing}
  pure lastSeenSig

setLastSeenSig :: SigInfo -> ConvertTestM ()
setLastSeenSig info = State.modify' $ \state -> state{lastSeenSig = Just info}

getNextTestName :: ConvertTestM (LocatedN RdrName)
getNextTestName = do
  ConvertTestState{allTests} <- State.get
  pure $ mkLRdrName $ testIdentifier (length allTests)

addTestInfo :: TestInfo -> ConvertTestM ()
addTestInfo testInfo =
  State.modify' $ \state -> state{allTests = allTests state Seq.|> testInfo}

extractResourceArgs :: [LPat GhcPs] -> ConvertTestM ([ResourceInfo], [LPat GhcPs])
extractResourceArgs args = do
  ConvertTestEnv{allResources} <- State.gets convertTestEnv
  pure $ extractResourceArgs' allResources args

{----- Utilities -----}

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
