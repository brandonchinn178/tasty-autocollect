{- AUTOCOLLECT.TEST -}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 902
#define __TEST_CONSTRUCTOR_WITH_TYPE_ARGS__ True
#else
#define __TEST_CONSTRUCTOR_WITH_TYPE_ARGS__ False
#endif

module Test.Tasty.AutoCollect.ConvertTestTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Control.Monad (forM_)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Predicates
import Test.Predicates.HUnit
import Test.Tasty.HUnit
import Text.Printf (printf)

import TestUtils.Golden
import TestUtils.Integration
import TestUtils.Predicates

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
        [ "test = testCase \"test\" $ 1 @?= 1"
        ]
  where
    testFile =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module Test where"
      , "import Prelude ()"
      , "import Test.Tasty.HUnit"
      , "test = testCase \"a test\" $ 1 @?= 1"
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

test_batch =
  [ testCase ("test runs with " <> label <> " as an argument") $
    assertSuccess_ . runTest $
      [ "test = foo " <> arg <> " $ return ()"
      , ""
      , "foo :: a -> Assertion -> TestTree"
      , "foo _ = testCase \"test helper\""
      , extraCode
      ]
  | (label, arg, extraCode) <-
      catMaybes
        [ test "literal int" "1" simple
        , test "literal float" "1.5" simple
        , test "literal empty list" "[]" simple
        , test "literal list" "[1,2,3]" simple
        , test "literal tuple" "(1, True)" simple
        , test "constructor" "(Just True)" simple
        , test "infix constructor" "(1 :+ 2)" (withExtra "data Foo = (:+) Int Int")
        , test "record constructor" "Foo{a = 1}" (withExtra "data Foo = Foo{a :: Int}")
        , test "constructor with type args" "(Just @Int 1)" (onlyWhen __TEST_CONSTRUCTOR_WITH_TYPE_ARGS__)
        , test "type signature" "(1 :: Int)" simple
        ]
  ]
  where
    test label arg f = f $ Just (label, arg, "" :: Text)
    simple = id
    withExtra extraCode = fmap (\(label, arg, _) -> (label, arg, extraCode))
    onlyWhen b = if b then id else const Nothing

test_batch =
  [ testCase "plugin propagates constructor type args correctly" $ do
    (_, stderr) <-
      assertAnyFailure . runTest $
        [ "test = foo (Just @Int True) \"a test\" $ return ()"
        , "  where foo = const testCase"
        ]
    stderr @?~ hasSubstr "Couldn't match expected type ‘Int’ with actual type ‘Bool’"
  | __TEST_CONSTRUCTOR_WITH_TYPE_ARGS__
  ]

test = testCase "generated test keeps where clause" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test = testCase \"a test\" $ constant @?= 42"
      , "  where"
      , "    constant = 42"
      ]
  getTestLines stdout @?~ containsStripped (eq "a test: OK")

test = testCase "test arguments can be defined in where clause" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test = testCase label $ constant @?= 42"
      , "  where"
      , "    label = \"constant is \" ++ show constant"
      , ""
      , "constant :: Int"
      , "constant = 42"
      ]
  getTestLines stdout @?~ containsStripped (eq "constant is 42: OK")

test = testCase "test can be defined with arbitrary testers" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test = boolTestCase \"this is a successful test\" $ 10 > 2"
      , ""
      , "boolTestCase :: TestName -> Bool -> TestTree"
      , "boolTestCase name x = testCase name $ assertBool \"assertion failed\" x"
      ]
  getTestLines stdout @?~ containsStripped (eq "this is a successful test: OK")

test = testCase "test can be defined with arbitrary testers in where clause" $ do
  (stdout, _) <-
    assertSuccess . runTest $
      [ "test = boolTestCase \"this is a successful test\" $ 10 > 2"
      , "  where"
      , "    boolTestCase :: TestName -> Bool -> TestTree"
      , "    boolTestCase name x = testCase name $ assertBool \"assertion failed\" x"
      ]
  getTestLines stdout @?~ containsStripped (eq "this is a successful test: OK")

test =
  testCase "testers can have any number of arguments" $
    assertSuccess_ $
      runTest $
        map Text.pack $
          concatMap mkTest [1 .. 10]
  where
    -- test = fooX "X args" 1 2 3 ... $ return ()
    --   where
    --     fooX name _ _ _ ... = testCase name
    mkTest arity =
      [ printf "test = foo%d \"%d args\" %s $ return ()" arity arity (mkArgs arity)
      , printf "  where"
      , printf "    foo%d name %s = testCase name" arity (mkPatterns arity)
      ]
    mkArgs arity = concatMap (\x -> show x <> " ") [1 .. arity]
    mkPatterns arity = concat $ replicate arity "_ "

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
  testCase "test file can contain multi-function signature" $
    assertSuccess_ . runTest $
      [ "test = testCase \"test\" $ timesTen 1 @?= timesFive 2"
      , ""
      , "timesTen, timesFive :: Int -> Int"
      , "timesTen = (* 10)"
      , "timesFive = (* 5)"
      ]

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

test = testGolden "test_batch fails when given arguments" "test_batch_args.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test_batch \"some name\" = []"
      ]
  return stderr

test = testGolden "test_batch fails when specifying wrong type" "test_batch_type.golden" $ do
  (_, stderr) <-
    assertAnyFailure . runTest $
      [ "test_batch :: Int"
      , "test_batch = []"
      ]
  return stderr
