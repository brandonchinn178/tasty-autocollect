{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.AutoCollect.GenerateMainTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Predicates
import Test.Predicates.HUnit
import Test.Tasty.HUnit

import TestUtils.Golden
import TestUtils.Integration
import TestUtils.Predicates

test =
  testCase "allows omitting all configuration" $
    assertSuccess_ $
      runMain ["{- AUTOCOLLECT.MAIN -}"]

test = testCase "searches recursively" $ do
  (stdout, _) <-
    assertSuccess . runMainWith (addFiles [("A/B/C/X/Y/Z.hs", testFile)]) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = modules"
      , "-}"
      ]
  getTestLines stdout @?~ containsStripped (eq "A.B.C.X.Y.Z")
  where
    testFile =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module A.B.C.X.Y.Z where"
      , "import Test.Tasty.HUnit"
      , "test = testCase \"test\" $ return ()"
      ]

test =
  testCase "ignores binary files" $
    assertSuccess_ $
      runMainWith
        ( \proj ->
            proj
              { preRunCallback = \tmpdir ->
                  ByteString.writeFile
                    (tmpdir </> "binary-file")
                    (ByteString.pack [0 ..])
              }
        )
        ["{- AUTOCOLLECT.MAIN -}"]

test_batch =
  [ testGolden
    ("output for group_type = " <> groupType <> " is as expected")
    ("output_group_type_" <> groupType <> ".golden")
    $ fmap (normalizeTestOutput . fst) . assertSuccess . runMainWith (setTestFiles testFiles)
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
      , "test = testCase \"test #1 for " <> ident <> "\" $ return ()"
      , "test = testCase \"test #2 for " <> ident <> "\" $ return ()"
      ]

test = testCase "generateMain orders test modules alphabetically" $ do
  (stdout, _) <-
    assertSuccess . runMainWith (setTestFiles testFiles) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = modules"
      , "-}"
      ]
  getTestLines stdout
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
      , "test = testCase \"test\" $ return ()"
      ]

test = testCase "allows stripping suffix from test modules" $ do
  (stdout, _) <-
    assertSuccess . runMainWith (setTestFiles testFiles) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = modules"
      , "strip_suffix = Foo"
      , "-}"
      ]
  getTestLines stdout
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
      , "test = testCase \"test\" $ return ()"
      ]

test = testCase "suffix is stripped before building module tree" $ do
  (stdout, _) <-
    assertSuccess . runMainWith (setTestFiles testFiles) $
      [ "{- AUTOCOLLECT.MAIN"
      , "group_type = tree"
      , "strip_suffix = Test"
      , "-}"
      ]
  getTestLines stdout
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
          , "test = testCase \"test1\" $ return ()"
          ]
        )
      ,
        ( "A/B/C/DTest.hs"
        ,
          [ "{- AUTOCOLLECT.TEST -}"
          , "module A.B.C.DTest where"
          , "import Test.Tasty.HUnit"
          , "test = testCase \"test2\" $ return ()"
          ]
        )
      ]

test = testCase "allows adding extra ingredients" $ do
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

test = testCase "gives informative error when ingredient lacks module" $ do
  (_, stderr) <-
    assertAnyFailure . runMain $
      [ "{- AUTOCOLLECT.MAIN"
      , "ingredients = myIngredient"
      , "-}"
      ]
  getTestLines stderr @?~ containsStripped (eq "Ingredient needs to be fully qualified: myIngredient")

test = testCase "allows disabling default tasty ingredients" $ do
  (_, stderr) <-
    assertAnyFailure . runMain $
      [ "{- AUTOCOLLECT.MAIN"
      , "ingredients_override = true"
      , "-}"
      ]
  stderr @?~ startsWith "No ingredients agreed to run."

test = testCase "allows overriding suite name" $ do
  (stdout, _) <-
    assertSuccess . runMain $
      [ "{- AUTOCOLLECT.MAIN"
      , "suite_name = my-test-suite"
      , "-}"
      ]
  stdout @?~ startsWith "my-test-suite"

test = testCase "allows customizing main module" $ do
  (stdout, _) <-
    assertSuccess . runMainWith (setTestFiles testFiles) $
      [ "{- AUTOCOLLECT.MAIN"
      , "custom_main = true"
      , "group_type = modules"
      , "strip_suffix = Test"
      , "-}"
      , ""
      , "{- AUTOCOLLECT.MAIN.imports -}"
      , "import Test.Tasty"
      , ""
      , "main :: IO ()"
      , "main = do"
      , "  putStrLn \"hello world!\""
      , "  defaultMain $ testGroup \"my tests\" tests"
      , "  where"
      , "    tests = id {- AUTOCOLLECT.MAIN.tests -}"
      ]
  getTestLines stdout
    @?~ startsWith
      [ "hello world!"
      , "my tests"
      , "  A"
      , "    test: OK"
      , "  A.A"
      , "    test: OK"
      , "  B"
      , "    test: OK"
      ]
  where
    testFiles =
      [ ("A.hs", testFile "A")
      , ("A/ATest.hs", testFile "A.ATest")
      , ("BTest.hs", testFile "BTest")
      ]
    testFile moduleName =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module " <> moduleName <> " where"
      , "import Test.Tasty.HUnit"
      , "test = testCase \"test\" $ return ()"
      ]

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
      , preRunCallback = \_ -> pure ()
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
        , "test = testCase \"a test in " <> moduleName <> "\" $ return ()"
        ]
      )
