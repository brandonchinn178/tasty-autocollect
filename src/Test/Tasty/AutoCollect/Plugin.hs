{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef)
import Data.List (intercalate, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Driver.Main (getHscEnv)
import GHC.Hs (
  GhcPs,
  GRHS (..),
  GRHSs (..),
  HsBindLR (..),
  HsConDetails (..),
  HsDecl (..),
  HsExpr (..),
  HsImplicitBndrs (..),
  HsLocalBindsLR (..),
  HsMatchContext (..),
  HsModule (..),
  HsPatSigType (..),
  HsTupArg (..),
  HsType (..),
  HsWildCardBndrs (..),
  IE (..),
  IEWrappedName (..),
  LHsDecl,
  LHsExpr,
  LHsLocalBinds,
  LHsSigWcType,
  LHsType,
  LPat,
  Match (..),
  MatchGroup (..),
  NoExtField (..),
  Pat (..),
  Sig (..),
 )
import GHC.Plugins hiding (getHscEnv)
import GHC.Parser.Annotation (
  AnnotationComment (..),
  ApiAnns (..),
  getAnnotationComments,
 )
import GHC.Settings (ToolSettings (..))
import GHC.Types.Name.Cache (NameCache)
import qualified GHC.Types.Name.Occurrence as NameSpace (varName)
import qualified Language.Haskell.TH as TH
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree)

plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , pluginRecompile = purePlugin
    , parsedResultAction = \_ _ modl -> do
        nameCache <- hsc_NC <$> getHscEnv
        testTreeName <-
          liftIO $
            thNameToGhcNameIO nameCache ''TestTree >>=
              maybe (autocollectError "Could not get Name for TestTree") return

        return $
          case getModuleType modl of
            Just ModuleMain -> transformMainModule modl
            Just ModuleTest -> transformTestModule testTreeName modl
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
transformTestModule :: Name -> HsParsedModule -> HsParsedModule
transformTestModule testTreeName parsedModl = parsedModl{hpm_module = updateModule <$> hpm_module parsedModl}
  where
    getCommentsAt = getAnnotationComments (hpm_annotations parsedModl)

    updateModule modl =
      let (decls, testNames) = runConvertTestM $ mapM (convertTest testTreeName) $ hsmodDecls modl
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
      case parseAutoCollectAnn loc of
        Just AutoCollectTestExport -> Just (getLoc loc)
        _ -> Nothing
    exportIE = IEVar NoExtField $ genLoc $ IEName testListName

    -- Generate the `tests` list
    mkTestsList :: [Located RdrName] -> [LHsDecl GhcPs]
    mkTestsList testNames =
      let testList = ExplicitList NoExtField Nothing $ map (genLoc . HsVar NoExtField) testNames
       in
        [ genLoc $ genFuncSig testListName $ genLoc $ HsListTy NoExtField $ toTestTreeType testTreeName
        , genLoc $ genFuncDecl testListName [] (genLoc testList) Nothing
        ]

{- |
If the given declaration is a test, return the converted test, or otherwise
return it unmodified
-}
convertTest :: Name -> LHsDecl GhcPs -> ConvertTestM (LHsDecl GhcPs)
convertTest testTreeName loc =
  case unLoc loc of
    -- e.g. test_testCase :: Assertion
    -- =>   test1 :: TestTree
    SigD _ (TypeSig _ [funcName] ty)
      | Just tester <- getTester funcName -> do
          testName <- getNextTestName
          setLastSeenSig
            SigInfo
              { testerName = tester
              , testName
              , testType = ty
              }
          pure (genFuncSig testName (toTestTreeType testTreeName) <$ loc)
    -- e.g. test_testCase "test name" = <body>
    -- =>   test1 = testCase "test name" (<body> :: Assertion)
    ValD _ (FunBind _ funcName funcMatchGroup _)
      | Just tester <- getTester funcName -> do
          (testName, funcBodyType) <- getLastSeenSig >>= \case
            Nothing -> autocollectError $ "Found test without type signature at " ++ getSpanLine funcName
            Just SigInfo{..} -> pure (testName, testType)

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

          -- tester funcArgs (funcBody :: mType)
          let testBody =
                exprApply . concat $
                  [ [genLoc $ HsVar NoExtField $ mkRdrName tester]
                  , map patternToExpr funcArgs
                  , [genLoc $ ExprWithTySig NoExtField funcBody funcBodyType]
                  ]

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
    VarPat _ name -> error "VarPat" name
    LazyPat{} -> unsupported "lazy patterns"
    AsPat{} -> unsupported "as patterns"
    ParPat _ p -> genLoc $ HsPar NoExtField $ patternToExpr p
    BangPat{} -> unsupported "bang patterns"
    ListPat _ ps -> genLoc $ ExplicitList NoExtField Nothing $ map patternToExpr ps
    TuplePat _ ps boxity -> genLoc $ ExplicitTuple NoExtField (map (genLoc . Present NoExtField . patternToExpr) ps) boxity
    SumPat{} -> unsupported "anonymous sum patterns"
    ConPat _ conName conDetails ->
      case conDetails of
        PrefixCon args -> exprApply $ genLoc (HsVar NoExtField conName) : map patternToExpr args
        RecCon fields -> genLoc $ RecordCon NoExtField conName $ patternToExpr <$> fields
        InfixCon l r -> exprApply $ genLoc (HsVar NoExtField conName) : map patternToExpr [l, r]
    ViewPat{} -> unsupported "view patterns"
    SplicePat _ splice -> genLoc $ HsSpliceE NoExtField splice
    LitPat _ lit -> genLoc $ HsLit NoExtField lit
    NPat _ lit _ _ -> genLoc $ HsOverLit NoExtField (unLoc lit)
    NPlusKPat{} -> unsupported "n+k patterns"
    SigPat _ p (HsPS _ ty) -> genLoc $ ExprWithTySig NoExtField (patternToExpr p) $ genHsWC (genLoc (unLoc ty))
  where
    unsupported label = autocollectError $ label ++ " unsupported as test argument at " ++ getSpanLine lpat

-- | Identifier for the generated `tests` list.
testListName :: Located RdrName
testListName = mkRdrName testListIdentifier

getTester :: Located RdrName -> Maybe String
getTester = stripPrefix "test_" . fromRdrName

toTestTreeType :: Name -> LHsType GhcPs
toTestTreeType = genLoc . HsTyVar NoExtField NotPromoted . genLoc . getRdrName

autocollectError :: String -> a
autocollectError msg =
  pgmError . unlines $
    [ ""
    , "******************** tasty-autocollect failure ********************"
    , msg
    ]

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
  let nextTestName = mkRdrName $ testIdentifier (length allTests)
  State.put state{allTests = allTests Seq.|> nextTestName}
  pure nextTestName

{----- Identifiers -----}

testListIdentifier :: String
testListIdentifier = "tasty_autocollect_tests"

testIdentifier :: Int -> String
testIdentifier x = "tasty_autocollect_test_" ++ show x

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

-- TODO: link to merge request in GHC repo
thNameToGhcNameIO :: IORef NameCache -> TH.Name -> IO (Maybe Name)
thNameToGhcNameIO cache name =
  fmap fst
    . runCoreM
        hsc_env
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

    hsc_env =
      HscEnv
        { hsc_dflags = dflags
        , hsc_targets = unused "hsc_targets"
        , hsc_mod_graph = unused "hsc_mod_graph"
        , hsc_IC = unused "hsc_IC"
        , hsc_HPT = unused "hsc_HPT"
        , hsc_EPS = strict $ unsafePerformIO $ newIORef eps
        , hsc_NC = cache
        , hsc_FC = strict $ unsafePerformIO $ newIORef emptyInstalledModuleEnv
        , hsc_type_env_var = unused "hsc_type_env_var"
        , hsc_interp = unused "hsc_interp"
        , hsc_dynLinker = unused "hsc_dynLinker"
        }
    dflags =
      DynFlags
        { ghcMode = unused "DynFlags.ghcMode"
        , ghcLink = unused "DynFlags.ghcLink"
        , hscTarget = unused "DynFlags.hscTarget"
        , ghcNameVersion = strict $ GhcNameVersion "" ""
        , fileSettings =
            strict
              FileSettings
                { fileSettings_ghcUsagePath = unused "fileSettings_ghcUsagePath"
                , fileSettings_ghciUsagePath = unused "fileSettings_ghciUsagePath"
                , fileSettings_toolDir = unused "fileSettings_toolDir"
                , fileSettings_topDir = unused "fileSettings_topDir"
                , fileSettings_tmpDir = unused "fileSettings_tmpDir"
                , fileSettings_globalPackageDatabase = unused "fileSettings_globalPackageDatabase"
                }
        , targetPlatform = unused "DynFlags.targetPlatform"
        , toolSettings =
            strict
              ToolSettings
                { toolSettings_ldSupportsCompactUnwind = unused "toolSettings_ldSupportsCompactUnwind"
                , toolSettings_ldSupportsBuildId = unused "toolSettings_ldSupportsBuildId"
                , toolSettings_ldSupportsFilelist = unused "toolSettings_ldSupportsFilelist"
                , toolSettings_ldIsGnuLd = unused "toolSettings_ldIsGnuLd"
                , toolSettings_ccSupportsNoPie = unused "toolSettings_ccSupportsNoPie"
                , toolSettings_pgm_L = unused "toolSettings_pgm_L"
                , toolSettings_pgm_P = unused "toolSettings_pgm_P"
                , toolSettings_pgm_F = unused "toolSettings_pgm_F"
                , toolSettings_pgm_c = unused "toolSettings_pgm_c"
                , toolSettings_pgm_a = unused "toolSettings_pgm_a"
                , toolSettings_pgm_l = unused "toolSettings_pgm_l"
                , toolSettings_pgm_lm = unused "toolSettings_pgm_lm"
                , toolSettings_pgm_dll = unused "toolSettings_pgm_dll"
                , toolSettings_pgm_T = unused "toolSettings_pgm_T"
                , toolSettings_pgm_windres = unused "toolSettings_pgm_windres"
                , toolSettings_pgm_libtool = unused "toolSettings_pgm_libtool"
                , toolSettings_pgm_ar = unused "toolSettings_pgm_ar"
                , toolSettings_pgm_otool = unused "toolSettings_pgm_otool"
                , toolSettings_pgm_install_name_tool = unused "toolSettings_pgm_install_name_tool"
                , toolSettings_pgm_ranlib = unused "toolSettings_pgm_ranlib"
                , toolSettings_pgm_lo = unused "toolSettings_pgm_lo"
                , toolSettings_pgm_lc = unused "toolSettings_pgm_lc"
                , toolSettings_pgm_lcc = unused "toolSettings_pgm_lcc"
                , toolSettings_pgm_i = unused "toolSettings_pgm_i"
                , toolSettings_opt_L = unused "toolSettings_opt_L"
                , toolSettings_opt_P = unused "toolSettings_opt_P"
                , toolSettings_opt_P_fingerprint = unused "toolSettings_opt_P_fingerprint"
                , toolSettings_opt_F = unused "toolSettings_opt_F"
                , toolSettings_opt_c = unused "toolSettings_opt_c"
                , toolSettings_opt_cxx = unused "toolSettings_opt_cxx"
                , toolSettings_opt_a = unused "toolSettings_opt_a"
                , toolSettings_opt_l = unused "toolSettings_opt_l"
                , toolSettings_opt_lm = unused "toolSettings_opt_lm"
                , toolSettings_opt_windres = unused "toolSettings_opt_windres"
                , toolSettings_opt_lo = unused "toolSettings_opt_lo"
                , toolSettings_opt_lc = unused "toolSettings_opt_lc"
                , toolSettings_opt_lcc = unused "toolSettings_opt_lcc"
                , toolSettings_opt_i = unused "toolSettings_opt_i"
                , toolSettings_extraGccViaCFlags = unused "toolSettings_extraGccViaCFlags"
                }
        , platformMisc =
            strict
              PlatformMisc
                { platformMisc_targetPlatformString = unused "platformMisc_targetPlatformString"
                , platformMisc_ghcWithInterpreter = unused "platformMisc_ghcWithInterpreter"
                , platformMisc_ghcWithSMP = unused "platformMisc_ghcWithSMP"
                , platformMisc_ghcRTSWays = unused "platformMisc_ghcRTSWays"
                , platformMisc_libFFI = unused "platformMisc_libFFI"
                , platformMisc_ghcThreaded = unused "platformMisc_ghcThreaded"
                , platformMisc_ghcDebugged = unused "platformMisc_ghcDebugged"
                , platformMisc_ghcRtsWithLibdw = unused "platformMisc_ghcRtsWithLibdw"
                , platformMisc_llvmTarget = unused "platformMisc_llvmTarget"
                }
        , platformConstants = unused "DynFlags.platformConstants"
        , rawSettings = unused "DynFlags.rawSettings"
        , llvmConfig = unused "DynFlags.llvmConfig"
        , verbosity = 0
        , optLevel = unused "DynFlags.optLevel"
        , debugLevel = unused "DynFlags.debugLevel"
        , simplPhases = unused "DynFlags.simplPhases"
        , maxSimplIterations = unused "DynFlags.maxSimplIterations"
        , ruleCheck = unused "DynFlags.ruleCheck"
        , inlineCheck = unused "DynFlags.inlineCheck"
        , strictnessBefore = unused "DynFlags.strictnessBefore"
        , parMakeCount = unused "DynFlags.parMakeCount"
        , enableTimeStats = unused "DynFlags.enableTimeStats"
        , ghcHeapSize = unused "DynFlags.ghcHeapSize"
        , maxRelevantBinds = unused "DynFlags.maxRelevantBinds"
        , maxValidHoleFits = unused "DynFlags.maxValidHoleFits"
        , maxRefHoleFits = unused "DynFlags.maxRefHoleFits"
        , refLevelHoleFits = unused "DynFlags.refLevelHoleFits"
        , maxUncoveredPatterns = unused "DynFlags.maxUncoveredPatterns"
        , maxPmCheckModels = unused "DynFlags.maxPmCheckModels"
        , simplTickFactor = unused "DynFlags.simplTickFactor"
        , specConstrThreshold = unused "DynFlags.specConstrThreshold"
        , specConstrCount = unused "DynFlags.specConstrCount"
        , specConstrRecursive = unused "DynFlags.specConstrRecursive"
        , binBlobThreshold = unused "DynFlags.binBlobThreshold"
        , liberateCaseThreshold = unused "DynFlags.liberateCaseThreshold"
        , floatLamArgs = unused "DynFlags.floatLamArgs"
        , liftLamsRecArgs = unused "DynFlags.liftLamsRecArgs"
        , liftLamsNonRecArgs = unused "DynFlags.liftLamsNonRecArgs"
        , liftLamsKnown = unused "DynFlags.liftLamsKnown"
        , cmmProcAlignment = unused "DynFlags.cmmProcAlignment"
        , historySize = unused "DynFlags.historySize"
        , importPaths = unused "DynFlags.importPaths"
        , mainModIs = unused "DynFlags.mainModIs"
        , mainFunIs = unused "DynFlags.mainFunIs"
        , reductionDepth = unused "DynFlags.reductionDepth"
        , solverIterations = unused "DynFlags.solverIterations"
        , homeUnitId = unused "DynFlags.homeUnitId"
        , homeUnitInstanceOfId = unused "DynFlags.homeUnitInstanceOfId"
        , homeUnitInstantiations = unused "DynFlags.homeUnitInstantiations"
        , ways = unused "DynFlags.ways"
        , splitInfo = unused "DynFlags.splitInfo"
        , objectDir = unused "DynFlags.objectDir"
        , dylibInstallName = unused "DynFlags.dylibInstallName"
        , hiDir = unused "DynFlags.hiDir"
        , hieDir = unused "DynFlags.hieDir"
        , stubDir = unused "DynFlags.stubDir"
        , dumpDir = unused "DynFlags.dumpDir"
        , objectSuf = unused "DynFlags.objectSuf"
        , hcSuf = unused "DynFlags.hcSuf"
        , hiSuf = unused "DynFlags.hiSuf"
        , hieSuf = unused "DynFlags.hieSuf"
        , canGenerateDynamicToo = unused "DynFlags.canGenerateDynamicToo"
        , dynObjectSuf = unused "DynFlags.dynObjectSuf"
        , dynHiSuf = unused "DynFlags.dynHiSuf"
        , outputFile = unused "DynFlags.outputFile"
        , dynOutputFile = unused "DynFlags.dynOutputFile"
        , outputHi = unused "DynFlags.outputHi"
        , dynLibLoader = unused "DynFlags.dynLibLoader"
        , dumpPrefix = unused "DynFlags.dumpPrefix"
        , dumpPrefixForce = unused "DynFlags.dumpPrefixForce"
        , ldInputs = unused "DynFlags.ldInputs"
        , includePaths = unused "DynFlags.includePaths"
        , libraryPaths = unused "DynFlags.libraryPaths"
        , frameworkPaths = unused "DynFlags.frameworkPaths"
        , cmdlineFrameworks = unused "DynFlags.cmdlineFrameworks"
        , rtsOpts = unused "DynFlags.rtsOpts"
        , rtsOptsEnabled = unused "DynFlags.rtsOptsEnabled"
        , rtsOptsSuggestions = unused "DynFlags.rtsOptsSuggestions"
        , hpcDir = unused "DynFlags.hpcDir"
        , pluginModNames = unused "DynFlags.pluginModNames"
        , pluginModNameOpts = unused "DynFlags.pluginModNameOpts"
        , frontendPluginOpts = unused "DynFlags.frontendPluginOpts"
        , cachedPlugins = unused "DynFlags.cachedPlugins"
        , staticPlugins = unused "DynFlags.staticPlugins"
        , hooks = unused "DynFlags.hooks"
        , depMakefile = unused "DynFlags.depMakefile"
        , depIncludePkgDeps = unused "DynFlags.depIncludePkgDeps"
        , depIncludeCppDeps = unused "DynFlags.depIncludeCppDeps"
        , depExcludeMods = unused "DynFlags.depExcludeMods"
        , depSuffixes = unused "DynFlags.depSuffixes"
        , packageDBFlags = unused "DynFlags.packageDBFlags"
        , ignorePackageFlags = unused "DynFlags.ignorePackageFlags"
        , packageFlags = unused "DynFlags.packageFlags"
        , pluginPackageFlags = unused "DynFlags.pluginPackageFlags"
        , trustFlags = unused "DynFlags.trustFlags"
        , packageEnv = unused "DynFlags.packageEnv"
        , unitDatabases = unused "DynFlags.unitDatabases"
        , unitState = unused "DynFlags.unitState"
        , filesToClean = unused "DynFlags.filesToClean"
        , dirsToClean = unused "DynFlags.dirsToClean"
        , nextTempSuffix = unused "DynFlags.nextTempSuffix"
        , generatedDumps = unused "DynFlags.generatedDumps"
        , dumpFlags = EnumSet.empty
        , generalFlags = unused "DynFlags.generalFlags"
        , warningFlags = unused "DynFlags.warningFlags"
        , fatalWarningFlags = unused "DynFlags.fatalWarningFlags"
        , language = unused "DynFlags.language"
        , safeHaskell = unused "DynFlags.safeHaskell"
        , safeInfer = unused "DynFlags.safeInfer"
        , safeInferred = unused "DynFlags.safeInferred"
        , thOnLoc = unused "DynFlags.thOnLoc"
        , newDerivOnLoc = unused "DynFlags.newDerivOnLoc"
        , deriveViaOnLoc = unused "DynFlags.deriveViaOnLoc"
        , overlapInstLoc = unused "DynFlags.overlapInstLoc"
        , incoherentOnLoc = unused "DynFlags.incoherentOnLoc"
        , pkgTrustOnLoc = unused "DynFlags.pkgTrustOnLoc"
        , warnSafeOnLoc = unused "DynFlags.warnSafeOnLoc"
        , warnUnsafeOnLoc = unused "DynFlags.warnUnsafeOnLoc"
        , trustworthyOnLoc = unused "DynFlags.trustworthyOnLoc"
        , extensions = unused "DynFlags.extensions"
        , extensionFlags = unused "DynFlags.extensionFlags"
        , ufCreationThreshold = unused "DynFlags.ufCreationThreshold"
        , ufUseThreshold = unused "DynFlags.ufUseThreshold"
        , ufFunAppDiscount = unused "DynFlags.ufFunAppDiscount"
        , ufDictDiscount = unused "DynFlags.ufDictDiscount"
        , ufDearOp = unused "DynFlags.ufDearOp"
        , ufVeryAggressive = unused "DynFlags.ufVeryAggressive"
        , maxWorkerArgs = unused "DynFlags.maxWorkerArgs"
        , ghciHistSize = unused "DynFlags.ghciHistSize"
        , log_action = unused "DynFlags.log_action"
        , dump_action = unused "DynFlags.dump_action"
        , trace_action = unused "DynFlags.trace_action"
        , flushOut = unused "DynFlags.flushOut"
        , flushErr = unused "DynFlags.flushErr"
        , ghcVersionFile = unused "DynFlags.ghcVersionFile"
        , haddockOptions = unused "DynFlags.haddockOptions"
        , ghciScripts = unused "DynFlags.ghciScripts"
        , pprUserLength = unused "DynFlags.pprUserLength"
        , pprCols = unused "DynFlags.pprCols"
        , useUnicode = unused "DynFlags.useUnicode"
        , useColor = unused "DynFlags.useColor"
        , canUseColor = unused "DynFlags.canUseColor"
        , colScheme = unused "DynFlags.colScheme"
        , profAuto = unused "DynFlags.profAuto"
        , interactivePrint = unused "DynFlags.interactivePrint"
        , nextWrapperNum = unused "DynFlags.nextWrapperNum"
        , sseVersion = unused "DynFlags.sseVersion"
        , bmiVersion = unused "DynFlags.bmiVersion"
        , avx = unused "DynFlags.avx"
        , avx2 = unused "DynFlags.avx2"
        , avx512cd = unused "DynFlags.avx512cd"
        , avx512er = unused "DynFlags.avx512er"
        , avx512f = unused "DynFlags.avx512f"
        , avx512pf = unused "DynFlags.avx512pf"
        , rtldInfo = unused "DynFlags.rtldInfo"
        , rtccInfo = unused "DynFlags.rtccInfo"
        , maxInlineAllocSize = unused "DynFlags.maxInlineAllocSize"
        , maxInlineMemcpyInsns = unused "DynFlags.maxInlineMemcpyInsns"
        , maxInlineMemsetInsns = unused "DynFlags.maxInlineMemsetInsns"
        , reverseErrors = unused "DynFlags.reverseErrors"
        , maxErrors = unused "DynFlags.maxErrors"
        , initialUnique = unused "DynFlags.initialUnique"
        , uniqueIncrement = unused "DynFlags.uniqueIncrement"
        , cfgWeightInfo = unused "DynFlags.cfgWeightInfo"
        }
    eps =
      EPS
        { eps_is_boot = strict mempty
        , eps_PIT = strict $ mkModuleEnv []
        , eps_free_holes = unused "EPS.eps_free_holes"
        , eps_PTE = strict mempty
        , eps_inst_env = strict mempty
        , eps_fam_inst_env = strict mempty
        , eps_rule_base = strict mempty
        , eps_ann_env = strict $ mkAnnEnv []
        , eps_complete_matches = strict mempty
        , eps_mod_fam_inst_env = strict $ mkModuleEnv []
        , eps_stats =
            strict $
              EpsStats
                { n_ifaces_in = strict 0
                , n_decls_in = strict 0
                , n_decls_out = strict 0
                , n_rules_in = strict 0
                , n_rules_out = strict 0
                , n_insts_in = strict 0
                , n_insts_out = strict 0
                }
        }
