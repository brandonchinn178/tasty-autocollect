module Test.Tasty.AutoCollect.GHC (
  -- * Builders
  genHsWC,
  genFuncSig,
  genFuncDecl,
  exprApply,

  -- * Located utilities
  genLoc,
  firstLocatedWhere,
  getSpanLine,

  -- * Name utilities
  mkRdrName,
  fromRdrName,
  thNameToGhcNameIO,
) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import GHC.IORef (IORef, newIORef)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Hs
import GHC.Plugins
import GHC.Settings (ToolSettings (..))
import GHC.Types.Name.Cache (NameCache)
import qualified GHC.Types.Name.Occurrence as NameSpace (varName)
import qualified Language.Haskell.TH as TH
import System.IO.Unsafe (unsafePerformIO)

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
