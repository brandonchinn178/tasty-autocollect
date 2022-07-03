{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Tasty.AutoCollect.ConvertTest (
  transformTestModule,
) where

import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Foldable (toList)
import Data.List (intercalate, stripPrefix)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Hs
import GHC.Parser.Annotation (
  getAnnotationComments,
 )
import GHC.Plugins

import Test.Tasty.AutoCollect.Constants
import Test.Tasty.AutoCollect.Error
import Test.Tasty.AutoCollect.ExternalNames
import Test.Tasty.AutoCollect.GHC

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
      let testList = ExplicitList NoExtField Nothing $ map (genLoc . HsVar NoExtField) testNames
       in [ genLoc $ genFuncSig testListName $ genLoc $ HsListTy NoExtField $ getTestTreeType names
          , genLoc $ genFuncDecl testListName [] (genLoc testList) Nothing
          ]

{- |
If the given declaration is a test, return the converted test, or otherwise
return it unmodified
-}
convertTest :: ExternalNames -> LHsDecl GhcPs -> ConvertTestM (LHsDecl GhcPs)
convertTest names loc =
  case unLoc loc of
    -- e.g. test_testCase :: Assertion
    -- =>   test1 :: TestTree
    SigD _ (TypeSig _ [funcName] ty)
      | Just tester <- parseTester funcName -> do
          testName <- getNextTestName
          setLastSeenSig
            SigInfo
              { tester
              , testName
              , testType = ty
              }
          pure (genFuncSig testName (getTestTreeType names) <$ loc)
    -- e.g. test_testCase "test name" = <body>
    -- =>   test1 = testCase "test name" (<body> :: Assertion)
    ValD _ (FunBind _ funcName funcMatchGroup _)
      | Just tester <- parseTester funcName -> do
          (testName, funcBodyType) <-
            getLastSeenSig >>= \case
              Nothing -> autocollectError $ "Found test without type signature at " ++ getSpanLine funcName
              Just SigInfo{tester = testerFromSig, ..}
                | tester == testerFromSig -> pure (testName, testType)
                | otherwise -> autocollectError $ "Found test with different type of signature: " ++ show (tester, testerFromSig)

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

          -- tester (...funcArgs) (funcBody :: mType)
          let testBody =
                mkHsApps (genLoc $ HsVar NoExtField $ genLoc $ fromTester names tester) $
                  map patternToExpr funcArgs ++ [genLoc $ ExprWithTySig NoExtField funcBody funcBodyType]

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
        PrefixCon args -> mkHsApps (genLoc (HsVar NoExtField conName)) $ map patternToExpr args
        RecCon fields -> genLoc $ RecordCon NoExtField conName $ patternToExpr <$> fields
        InfixCon l r -> mkHsApps (genLoc (HsVar NoExtField conName)) $ map patternToExpr [l, r]
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

data Tester
  = Tester String
  deriving (Show, Eq)

parseTester :: Located RdrName -> Maybe Tester
parseTester = fmap toIdentifier . stripPrefix "test_" . fromRdrName
  where
    toIdentifier = \case
      s -> Tester s

fromTester :: ExternalNames -> Tester -> RdrName
fromTester _ = \case
  Tester name -> mkRdrName name

getTestTreeType :: ExternalNames -> LHsType GhcPs
getTestTreeType = genLoc . HsTyVar NoExtField NotPromoted . genLoc . getRdrName . name_TestTree

{----- Test converter monad -----}

type ConvertTestM = State ConvertTestState

data ConvertTestState = ConvertTestState
  { lastSeenSig :: Maybe SigInfo
  , allTests :: Seq (Located RdrName)
  }

data SigInfo = SigInfo
  { tester :: Tester
  -- ^ The parsed tester
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
  let nextTestName = mkLRdrName $ testIdentifier (length allTests)
  State.put state{allTests = allTests Seq.|> nextTestName}
  pure nextTestName
