{- AUTOCOLLECT.TEST -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.ConvertTestTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Control.Monad (forM_)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Predicates
import Test.Predicates.HUnit
import Test.Tasty.HUnit
import Text.Printf (printf)

import TestUtils.Golden
import TestUtils.Integration
import TestUtils.Predicates

{----- General plugin tests -----}

test =
  testCase "plugin works without tasty installed" $
    assertSuccess_ $
      runTestWith
        ( \proj ->
            modifyFile "Test.hs" (filter (not . isTastyImport)) $
              proj{dependencies = filter (/= "tasty") (dependencies proj)}
        )
        [ "test = testCase \"test\" $ 1 @?= 1"
        ]
  where
    isTastyImport line =
      case Text.unpack <$> Text.stripPrefix "import Test.Tasty" line of
        -- not an import / import from non-tasty library
        Nothing -> False
        -- import from `Test.Tasty`
        Just "" -> True
        -- import from `Test.Tasty (...)` or `Test.Tasty hiding (...)`
        Just " " -> True
        -- import from `Test.Tasty.Foo` or `Test.TastyFoo`, which is ok
        _ -> False

test =
  testCase "plugin works without Prelude" $
    assertSuccess_ $
      runTestWith
        (modifyFile "Test.hs" (const testFile))
        []
  where
    testFile =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module Test where"
      , "import Prelude ()"
      , "import Test.Tasty.HUnit"
      , "test = testCase \"a test\" (1 @?= 1)"
      ]

test_batch =
  [ testCase ("plugin works when " ++ mkLabel ext) $
    assertSuccess_ $
      runTestWith
        (\proj -> proj{extraGhcArgs = maybeToList ext <> extraGhcArgs proj})
        [ "test = testCase \"1 = 1\" $ 1 @?= 1"
        ]
  | ext <-
      [ Just "-XOverloadedStrings"
      , Just "-XOverloadedLists"
      , Nothing
      ]
  ]
  where
    mkLabel = \case
      Nothing -> "no extensions are enabled"
      Just ext -> "enabling " <> Text.unpack ext

{----- Overall test file -----}

test = testCase "tests fail when omitting export comment" $ do
  (_, stderr) <-
    assertAnyFailure . runTestWith (modifyFile "Test.hs" (map removeExports)) $
      [ "test = testCase \"a test\" $ return ()"
      ]
  getTestLines stderr @?~ containsStripped (startsWith "Module ‘Test’ does not export")
  where
    removeExports s
      | "module " `Text.isPrefixOf` s = "module Test () where"
      | otherwise = s

test = testCase "test file can omit an explicit export list" $ do
  (stdout, _) <-
    assertSuccess . runTestWith (modifyFile "Test.hs" (map removeExports)) $
      [ "test = testCase \"a test\" $ return ()"
      ]
  getTestLines stdout @?~ containsStripped (eq "a test: OK")
  where
    removeExports s
      | "module " `Text.isPrefixOf` s = "module Test where"
      | otherwise = s

test =
  testCase "test file can contain multi-function signature" $
    assertSuccess_ . runTest $
      [ "test = testCase \"test\" $ timesTen 1 @?= timesFive 2"
      , ""
      , "timesTen, timesFive :: Int -> Int"
      , "timesTen = (* 10)"
      , "timesFive = (* 5)"
      ]

{----- Generated tests -----}

test = testCase "generated test keeps where clause" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test = testCase \"a test\" $ constant @?= 42"
      , "  where"
      , "    constant = 42"
      ]
  getTestLines stdout @?~ containsStripped (eq "a test: OK")

test =
  testCase "test may specify type" $
    assertSuccess_ . runTest $
      [ "test :: TestTree"
      , "test = testCase \"a test\" $ return ()"
      ]

test = testGolden "test fails when given arguments" "test_args.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test \"some name\" = testCase \"test\" $ return ()"
      ]
  return stderr

test = testGolden "test fails when specifying wrong type" "test_type.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test :: Int"
      , "test = testCase \"test\" $ return ()"
      ]
  return stderr

test = testCase "tests can omit type signatures" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test = testCase \"test 1\" $ return ()"
      , ""
      , "test = testCase \"test 2\" $ return ()"
      ]
  getTestLines stdout @?~ containsStripped (eq "test 1: OK")
  getTestLines stdout @?~ containsStripped (eq "test 2: OK")

test =
  testCase "tests may omit type after specifying a type prior" $
    assertSuccess_ . runQCTest $
      [ "test :: TestTree"
      , "test = testCase \"test 1\" $ return ()"
      , ""
      , "test = testCase \"test 2\" $ return ()"
      ]

{----- test_batch -----}

test = testCase "test_batch generates multiple tests" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test_batch ="
      , "  [ testCase (\"test #\" ++ show x) $ return ()"
      , "  | x <- [1 .. 5]"
      , "  ]"
      ]
  forM_ [1 .. 5 :: Int] $ \x ->
    getTestLines stdout @?~ containsStripped (eq . Text.pack $ printf "test #%d: OK" x)

test = testCase "test_batch includes where clause" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test_batch ="
      , "  [ testCase (label x) $ return ()"
      , "  | x <- [1 .. 5]"
      , "  ]"
      , "  where"
      , "    label x = \"test #\" ++ show x"
      ]
  forM_ [1 .. 5 :: Int] $ \x ->
    getTestLines stdout @?~ containsStripped (eq . Text.pack $ printf "test #%d: OK" x)

test =
  testCase "test_batch may specify type" $
    assertSuccess_ . runTest $
      [ "test_batch :: [TestTree]"
      , "test_batch = []"
      ]

test = testGolden "test_batch fails when given arguments" "test_batch_args.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test_batch \"some name\" = []"
      ]
  return stderr

test = testGolden "test_batch fails when specifying wrong type" "test_batch_type.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test_batch :: TestTree"
      , "test_batch = []"
      ]
  return stderr

{----- test_prop -----}

test =
  testCase "property tests may be written with test_prop" $ do
    (stdout, _) <-
      assertSuccess . runQCTest $
        [ "test_prop :: Positive Int -> [Int] -> Bool"
        , "test_prop \"take N returns at most N elements\" (Positive n) xs = length (take n xs) <= n"
        ]
    getTestLines stdout @?~ containsStripped (eq "take N returns at most N elements: OK")
    stdout @?~ hasSubstr "passed 100 tests"

test =
  testCase "test_prop may omit type" $
    assertSuccess_ . runQCTest $
      [ "test_prop \"test\" x = (x :: Int) === x"
      ]

test =
  testCase "test_prop may omit type after specifying a different type prior" $
    assertSuccess_ . runQCTest $
      [ "test_prop :: Property"
      , "test_prop \"test 1\" = 1 === 1"
      , ""
      , "test_prop \"test 2\" x = (x :: Int) === x"
      ]

test =
  testCase "test_prop uses any 'testProperty' function in scope" $ do
    (stdout, _) <-
      assertSuccess . runTest $
        [ "test_prop :: Int -> Bool"
        , "test_prop \"my property test\" x = x == x"
        , ""
        , "testProperty :: String -> (Int -> Bool) -> TestTree"
        , "testProperty name f = testCase name (f 1 @?= True)"
        ]
    getTestLines stdout @?~ containsStripped (eq "my property test: OK")

test =
  testGolden "test_prop fails when no arguments provided" "test_prop_no_args.golden" $ do
    (_, stderr) <- assertAnyFailure $ runTest ["test_prop = 1 === 1"]
    return stderr

test =
  testGolden "test_prop fails when non-string argument provided" "test_prop_bad_arg.golden" $ do
    (_, stderr) <- assertAnyFailure $ runTest ["test_prop 11 = True"]
    return stderr

test =
  testCase "test_prop works when -XOverloadedStrings is enabled" $
    assertSuccess_ $
      runTestWith
        ( \proj -> addQuickCheck $ proj{extraGhcArgs = "-XOverloadedStrings" : extraGhcArgs proj}
        )
        [ "import Test.Tasty.QuickCheck"
        , "test_prop \"a test\" = True"
        ]

runQCTest :: FileContents -> IO (ExitCode, Text, Text)
runQCTest = runTestWith addQuickCheck . ("import Test.Tasty.QuickCheck" :)

addQuickCheck :: GHCProject -> GHCProject
addQuickCheck proj = proj{dependencies = "tasty-quickcheck" : dependencies proj}

{----- tasty-expected-failure integration -----}

test =
  testGolden "expectFail succeeds when test fails" "test_expectFail_output.golden" $ do
    (stdout, _) <-
      assertSuccess . runTest $
        [ "test_expectFail = testCase \"failing test\" $ 1 @?= 2"
        ]
    return (normalizeTestOutput stdout)

test =
  testGolden "expectFailBecause succeeds when test fails" "test_expectFailBecause_output.golden" $ do
    (stdout, _) <-
      assertSuccess . runTest $
        [ "test_expectFailBecause \"some reason\" = testCase \"failing test\" $ 1 @?= 2"
        ]
    return (normalizeTestOutput stdout)

test =
  testGolden "ignoreTest succeeds when test fails" "test_ignoreTest_output.golden" $ do
    (stdout, _) <-
      assertSuccess . runTest $
        [ "test_ignoreTest = testCase \"failing test\" $ 1 @?= 2"
        ]
    return (normalizeTestOutput stdout)

test =
  testGolden "ignoreTestBecause succeeds when test fails" "test_ignoreTestBecause_output.golden" $ do
    (stdout, _) <-
      assertSuccess . runTest $
        [ "test_ignoreTestBecause \"some reason\" = testCase \"failing test\" $ 1 @?= 2"
        ]
    return (normalizeTestOutput stdout)

test =
  testGolden "expected-failure modifiers work on test_batch" "test_batch_expectFailBecause_output.golden" $ do
    (stdout, _) <-
      assertAnyFailure . runTest $
        [ "test_batch_expectFailBecause \"some reason\" ="
        , "  [ testCase (\"failing test #\" ++ show x) $ x @?= 1"
        , "  | x <- [1 .. 3 :: Int]"
        , "  ]"
        ]
    return (normalizeTestOutput stdout)

test =
  testCase "expected-failure modifiers work on test_prop" $ do
    (stdout, _) <-
      assertSuccess . runQCTest $
        [ "test_prop_expectFailBecause :: [Int] -> Bool"
        , "test_prop_expectFailBecause \"some reason\" \"my property\" xs = length xs < 0"
        ]
    getTestLines stdout @?~ containsStripped (eq "my property: FAIL (expected: some reason)")
