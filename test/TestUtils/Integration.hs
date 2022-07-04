{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestUtils.Integration (
  runTest,
  runTestWith,
  assertSuccess,
  assertSuccess_,
  assertAnyFailure,
  assertAnyFailure_,
  GHCProject (..),
  runghc,
  ExitCode (..),
) where

import Control.Monad (forM_, void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (ExitCode (..), proc, readProcess, setWorkingDir)

-- | Contents of a file broken up by lines.
type FileContents = [Text]

data GHCProject = GHCProject
  { dependencies :: [Text]
  , extraGhcArgs :: [Text]
  , files :: [(FilePath, FileContents)]
  , entrypoint :: FilePath
  , runArgs :: [Text]
  }

{- |
Run a test file with tasty-autocollect.

Automatically imports Test.Tasty and Test.Tasty.HUnit.
-}
runTest :: FileContents -> IO (ExitCode, Text, Text)
runTest = runTestWith id

-- | Same as 'runTest', except allows modifying the project before running.
runTestWith :: (GHCProject -> GHCProject) -> FileContents -> IO (ExitCode, Text, Text)
runTestWith f contents =
  runghc . f $
    GHCProject
      { dependencies = ["tasty", "tasty-hunit"]
      , extraGhcArgs = ["-F", "-pgmF=tasty-autocollect"]
      , files =
          [ ("Test.hs", testFilePrefix ++ contents)
          , ("Main.hs", ["{- AUTOCOLLECT.MAIN -}"])
          ]
      , entrypoint = "Main.hs"
      , runArgs = []
      }
  where
    testFilePrefix =
      [ "{- AUTOCOLLECT.TEST -}"
      , "module Test ({- AUTOCOLLECT.TEST.export -}) where"
      , "import Test.Tasty"
      , "import Test.Tasty.HUnit"
      ]

assertStatus :: (ExitCode -> Bool) -> IO (ExitCode, Text, Text) -> IO (Text, Text)
assertStatus isExpected testResult = do
  (code, stdout, stderr) <- testResult
  if isExpected code
    then return (stdout, stderr)
    else
      errorWithoutStackTrace . unlines $
        [ "Got: " ++ show code
        , "Stdout: " ++ Text.unpack stdout
        , "Stderr: " ++ Text.unpack stderr
        ]

assertSuccess :: IO (ExitCode, Text, Text) -> IO (Text, Text)
assertSuccess = assertStatus $ \case
  ExitSuccess -> True
  ExitFailure _ -> False

assertSuccess_ :: IO (ExitCode, Text, Text) -> IO ()
assertSuccess_ = void . assertSuccess

assertAnyFailure :: IO (ExitCode, Text, Text) -> IO (Text, Text)
assertAnyFailure = assertStatus $ \case
  ExitSuccess -> False
  ExitFailure _ -> True

assertAnyFailure_ :: IO (ExitCode, Text, Text) -> IO ()
assertAnyFailure_ = void . assertAnyFailure

-- | Compile and run the given project.
runghc :: GHCProject -> IO (ExitCode, Text, Text)
runghc GHCProject{..} =
  withSystemTempDirectory "tasty-autocollect-integration-test" $ \tmpdir -> do
    forM_ files $ \(fp, contents) -> Text.writeFile (tmpdir </> fp) (Text.unlines contents)

    let ghcArgs =
          concat
            [ ["-hide-all-packages"]
            , ["-package " <> dep | dep <- dependencies]
            , extraGhcArgs
            ]

    (code, stdout, stderr) <-
      readProcess $
        setWorkingDir tmpdir . proc "runghc" . concat $
          [ ["--"]
          , map Text.unpack ghcArgs
          , "--" : entrypoint : map Text.unpack runArgs
          ]

    let decode = TextL.toStrict . TextL.decodeUtf8
    return (code, decode stdout, decode stderr)
