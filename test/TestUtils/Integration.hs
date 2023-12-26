{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TestUtils.Integration (
  -- * Status assertions
  assertSuccess,
  assertSuccess_,
  assertAnyFailure,
  assertAnyFailure_,

  -- * GHCProject
  FileContents,
  GHCProject (..),
  addFiles,
  modifyFile,
  runghc,

  -- * Helpers
  runTest,
  runTestWith,
  getTestLines,
  normalizeTestOutput,

  -- * Re-exports
  ExitCode (..),
) where

import Control.Monad (forM_, void)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (
  ExitCode (..),
  proc,
  readProcess,
  setWorkingDir,
 )

import Test.Tasty.AutoCollect.Utils.Text (breakOnEnd)

assertStatus :: (ExitCode -> Bool) -> IO (ExitCode, Text, Text) -> IO (Text, Text)
assertStatus isExpected testResult = do
  (code, stdout, stderr) <- testResult
  if isExpected code
    then pure (stdout, stderr)
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

{----- GHCProject -----}

-- | Contents of a file broken up by lines.
type FileContents = [Text]

data GHCProject = GHCProject
  { dependencies :: [Text]
  , extraGhcArgs :: [Text]
  , files :: [(FilePath, FileContents)]
  , preRunCallback :: FilePath -> IO ()
  , entrypoint :: FilePath
  , runArgs :: [Text]
  }

addFiles :: [(FilePath, FileContents)] -> GHCProject -> GHCProject
addFiles newFiles proj = proj{files = files proj ++ newFiles}

modifyFile :: FilePath -> (FileContents -> FileContents) -> GHCProject -> GHCProject
modifyFile path f proj = proj{files = map modify (files proj)}
  where
    modify (fp, contents) = (fp, (if fp == path then f else id) contents)

-- | Compile and run the given project.
runghc :: GHCProject -> IO (ExitCode, Text, Text)
runghc GHCProject{..} =
  withSystemTempDirectory "tasty-autocollect-integration-test" $ \tmpdir -> do
    -- create files
    forM_ files $ \(fp, contents) -> do
      let testFile = tmpdir </> fp
      createDirectoryIfMissing True (takeDirectory testFile)
      Text.writeFile testFile (Text.unlines contents)

    -- run callback
    preRunCallback tmpdir

    -- run ghc
    let ghcArgs =
          concat
            [ ["-hide-all-packages"]
            , ["-package " <> dep | dep <- dependencies]
            , ["-package tasty-autocollect"]
            , extraGhcArgs
            ]
    (code, stdout, stderr) <-
      readProcess $
        setWorkingDir tmpdir . proc "runghc" . concat $
          [ "--" : map Text.unpack ghcArgs
          , "--" : entrypoint : map Text.unpack runArgs
          ]

    pure (code, decode stdout, decode stderr)
  where
    decode = TextL.toStrict . TextL.decodeUtf8

{----- Helpers -----}

-- | Run a test file with tasty-autocollect.
--
-- Automatically imports Test.Tasty and Test.Tasty.HUnit.
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
      , preRunCallback = \_ -> pure ()
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

-- | Get and normalize tasty output lines.
getTestLines :: Text -> [Text]
getTestLines = Text.lines . normalizeTestOutput

-- https://github.com/UnkindPartition/tasty/issues/341
normalizeTestOutput :: Text -> Text
normalizeTestOutput = Text.unlines . map normalize . Text.lines
  where
    normalize s
      | (pre, rest) <- breakOnEnd " (" s
      , Just inParens <- Text.stripSuffix ")" rest
      , Just (inParensNum, 's') <- Text.unsnoc inParens
      , [a, b] <- Text.splitOn "." inParensNum
      , Text.all isDigit a
      , Text.all isDigit b =
          pre
      | otherwise = s
