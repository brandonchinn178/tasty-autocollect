{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.Plugin (plugin) where

import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (intercalate, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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
  ImportDecl (..),
  ImportDeclQualifiedStyle (..),
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
import GHC.Plugins
import GHC.Parser.Annotation (
  AnnotationComment (..),
  ApiAnns (..),
  getAnnotationComments,
 )
import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)

plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ df ->
        pure $ df `gopt_set` Opt_KeepRawTokenStream
    , pluginRecompile = purePlugin
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
transformTestModule :: HsParsedModule -> HsParsedModule
transformTestModule parsedModl = parsedModl{hpm_module = updateModule <$> hpm_module parsedModl}
  where
    getCommentsAt = getAnnotationComments (hpm_annotations parsedModl)

    updateModule modl =
      let (decls, testNames) = runConvertTestM $ mapM convertTest $ hsmodDecls modl
       in modl
            { hsmodExports = updateExports <$> hsmodExports modl
            , hsmodImports = testTreeImport : hsmodImports modl
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

    -- Import 'Test.Tasty.TestTree'
    testTreeImport =
      genLoc $
        ImportDecl
          { ideclExt = NoExtField
          , ideclSourceSrc = NoSourceText
          , ideclName = genLoc $ mkModuleName "Test.Tasty"
          , ideclPkgQual = Nothing
          , ideclSource = NotBoot
          , ideclSafe = False
          , ideclQualified = QualifiedPre -- TODO: make qualified
          , ideclImplicit = False
          , ideclAs = Just (genLoc testTastyModule)
          , ideclHiding = Nothing
          }

    -- Generate the `tests` list
    mkTestsList :: [Located RdrName] -> [LHsDecl GhcPs]
    mkTestsList testNames =
      let testList = ExplicitList NoExtField Nothing $ map (genLoc . HsVar NoExtField) testNames
       in
        [ genLoc $ genFuncSig testListName $ genLoc $ HsListTy NoExtField testTreeType
        , genLoc $ genFuncDecl testListName [] (genLoc testList) Nothing
        ]

{- |
If the given declaration is a test, return the converted test, or otherwise
return it unmodified
-}
convertTest :: LHsDecl GhcPs -> ConvertTestM (LHsDecl GhcPs)
convertTest loc =
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
          pure (genFuncSig testName testTreeType <$ loc)
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

-- | ModuleName for 'Test.Tasty', renamed to ensure we're using the 'TestTree' we're importing ad-hoc.
testTastyModule :: ModuleName
testTastyModule = mkModuleName "TastyAutoCollect.Test.Tasty"

testTreeType :: LHsType GhcPs
testTreeType =
  genLoc . HsTyVar NoExtField NotPromoted $
    genLoc . mkRdrQual testTastyModule $
      mkOccName NameSpace.tcName "TestTree"

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
