{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GenerateMainTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Test.Predicates
import Test.Predicates.HUnit
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import TestUtils.Golden
import TestUtils.Integration
import TestUtils.Predicates

test_testCase :: Assertion
test_testCase "allows omitting all configuration" =
  assertSuccess_ $ runMain ["{- AUTOCOLLECT.MAIN -}"]

test_testCase :: Assertion
test_testCase "searches recursively" = do
  (stdout, _) <-
    assertSuccess . runMainWith (addFiles [("A/B/C/X/Y/Z.hs", testFile)]) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = modules"
      , "-}"
      ]
  Text.lines stdout @?~ contains (strippedEq "A.B.C.X.Y.Z")
  where
    testFile =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module A.B.C.X.Y.Z where"
      , "import Test.Tasty.HUnit"
      , "test_testCase :: Assertion"
      , "test_testCase \"test\" = return ()"
      ]

test_batch :: [TestTree]
test_batch =
  [ testGolden
    ("output for group_type = " <> groupType <> " is as expected")
    ("output_group_type_" <> groupType <> ".golden")
    $ fmap fst . assertSuccess . runMainWith (setTestFiles testFiles)
    $ [ "{- AUTOCOLLECT.MAIN"
      , "group_type = " <> Text.pack groupType
      , "-}"
      ]
  | groupType <- ["flat", "modules", "tree"]
  ]
  where
    testFiles =
      [ ("MyProject/Test/A.hs", testFile "MyProject.Test.A" "A")
      , ("MyProject/Test/A/X.hs", testFile "MyProject.Test.A.X" "AX")
      , ("MyProject/Test/A/Y.hs", testFile "MyProject.Test.A.Y" "AY")
      , ("MyProject/Test/A/Z.hs", testFile "MyProject.Test.A.Z" "AZ")
      , ("MyProject/Test/B.hs", testFile "MyProject.Test.B" "B")
      , ("MyProject/Test/C/A.hs", testFile "MyProject.Test.C.A" "CA")
      , ("MyProject/Test/C/B.hs", testFile "MyProject.Test.C.B" "CB")
      ]
    testFile moduleName ident =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module " <> moduleName <> " where"
      , "import Test.Tasty.HUnit"
      , "test_testCase :: Assertion"
      , "test_testCase \"test #1 for " <> ident <> "\" = return ()"
      , "test_testCase :: Assertion"
      , "test_testCase \"test #2 for " <> ident <> "\" = return ()"
      ]

-- test_batch "Golden test on stdout of generateMain for each group type" = ()

test_testCase :: Assertion
test_testCase "generateMain orders test modules alphabetically" = do
  (stdout, _) <-
    assertSuccess . runMainWith (setTestFiles testFiles) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = modules"
      , "-}"
      ]
  Text.lines stdout
    @?~ startsWith
      [ "Main.hs"
      , "  A"
      , "    test: OK"
      , "  A.A"
      , "    test: OK"
      , "  A.B"
      , "    test: OK"
      , "  B"
      , "    test: OK"
      , "  C"
      , "    test: OK"
      ]
  where
    testFiles =
      [ ("A.hs", testFile "A")
      , ("A/A.hs", testFile "A.A")
      , ("A/B.hs", testFile "A.B")
      , ("B.hs", testFile "B")
      , ("C.hs", testFile "C")
      ]
    testFile moduleName =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module " <> moduleName <> " where"
      , "import Test.Tasty.HUnit"
      , "test_testCase :: Assertion"
      , "test_testCase \"test\" = return ()"
      ]

test_testCase :: Assertion
test_testCase "allows stripping suffix from test modules" = do
  (stdout, _) <-
    assertSuccess . runMainWith (setTestFiles testFiles) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = modules"
      , "strip_suffix = Foo"
      , "-}"
      ]
  Text.lines stdout
    @?~ startsWith
      [ "Main.hs"
      , "  Tests.A"
      , "    test: OK"
      , "  Tests.B"
      , "    test: OK"
      ]
  where
    testFiles =
      [ ("Tests/AFoo.hs", testFile "Tests.AFoo")
      , ("Tests/BFoo.hs", testFile "Tests.BFoo")
      ]
    testFile moduleName =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module " <> moduleName <> " where"
      , "import Test.Tasty.HUnit"
      , "test_testCase :: Assertion"
      , "test_testCase \"test\" = return ()"
      ]

test_testCase :: Assertion
test_testCase "suffix is stripped before building module tree" = do
  (stdout, _) <-
    assertSuccess . runMainWith (setTestFiles testFiles) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = tree"
      , "strip_suffix = Test"
      , "-}"
      ]
  Text.lines stdout
    @?~ startsWith
      [ "Main.hs"
      , "  A"
      , "    B"
      , "      C"
      , "        test1:   OK" -- should be under the same "C" as the "C.DTest" test module
      , "        D"
      , "          test2: OK"
      ]
  where
    testFiles =
      [
        ( "A/B/CTest.hs"
        ,
          [ "{- AUTOCOLLECT.TEST -}"
          , "module A.B.CTest where"
          , "import Test.Tasty.HUnit"
          , "test_testCase :: Assertion"
          , "test_testCase \"test1\" = return ()"
          ]
        )
      ,
        ( "A/B/C/DTest.hs"
        ,
          [ "{- AUTOCOLLECT.TEST -}"
          , "module A.B.C.DTest where"
          , "import Test.Tasty.HUnit"
          , "test_testCase :: Assertion"
          , "test_testCase \"test2\" = return ()"
          ]
        )
      ]

test_testCase :: Assertion
test_testCase "allows adding extra ingredients" = do
  (stdout, _) <-
    assertSuccess . runMainWith (addFiles [("MyIngredient.hs", ingredientFile)]) $
      [ "{- AUTOCOLLECT.MAIN"
      , "ingredients = MyIngredient.sayHelloAndExit"
      , "-}"
      ]
  stdout @?= "Hello!\n"
  where
    ingredientFile =
      [ "module MyIngredient where"
      , "import Test.Tasty.Ingredients"
      , "sayHelloAndExit :: Ingredient"
      , "sayHelloAndExit = TestManager [] $ \\_ _ -> Just $"
      , "  putStrLn \"Hello!\" >> return True"
      ]

test_testCase :: Assertion
test_testCase "gives informative error when ingredient lacks module" = do
  (_, stderr) <-
    assertAnyFailure . runMain $
      [ "{- AUTOCOLLECT.MAIN"
      , "ingredients = myIngredient"
      , "-}"
      ]
  Text.lines stderr @?~ contains (eq "Ingredient needs to be fully qualified: myIngredient")

test_testCase :: Assertion
test_testCase "allows disabling default tasty ingredients" = do
  (_, stderr) <-
    assertAnyFailure . runMain $
      [ "{- AUTOCOLLECT.MAIN"
      , "ingredients_override = true"
      , "-}"
      ]
  stderr @?~ startsWith "No ingredients agreed to run."

test_testCase :: Assertion
test_testCase "allows overriding suite name" = do
  (stdout, _) <-
    assertSuccess . runMain $
      [ "{- AUTOCOLLECT.MAIN"
      , "suite_name = my-test-suite"
      , "-}"
      ]
  stdout @?~ startsWith "my-test-suite"

{----- Helpers -----}

setTestFiles :: [(FilePath, FileContents)] -> GHCProject -> GHCProject
setTestFiles testFiles proj =
  proj
    { files = filter ((== "Main.hs") . fst) (files proj) ++ testFiles
    }

runMain :: FileContents -> IO (ExitCode, Text, Text)
runMain = runMainWith id

runMainWith :: (GHCProject -> GHCProject) -> FileContents -> IO (ExitCode, Text, Text)
runMainWith f mainFile =
  runghc . f $
    GHCProject
      { dependencies = ["tasty", "tasty-hunit"]
      , extraGhcArgs = ["-F", "-pgmF=tasty-autocollect"]
      , files =
          [ ("Main.hs", mainFile)
          , testFile "FooTest"
          , testFile "BarTest"
          ]
      , entrypoint = "Main.hs"
      , runArgs = []
      }
  where
    testFile moduleName =
      ( Text.unpack moduleName <> ".hs"
      ,
        [ "{- AUTOCOLLECT.TEST -}"
        , "module " <> moduleName <> " ({- AUTOCOLLECT.TEST.export -}) where"
        , "import Test.Tasty"
        , "import Test.Tasty.HUnit"
        , ""
        , "test_testCase :: Assertion"
        , "test_testCase \"a test in " <> moduleName <> "\" = return ()"
        ]
      )
