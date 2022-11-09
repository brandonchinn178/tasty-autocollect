{-# LANGUAGE LambdaCase #-}

{- |
A preprocessor that registers tasty-autocollect in a test suite.

We need to use a preprocessor for Main.hs because GHC plugins don't
seem to support dynamically registering other modules as imports (GHC
already knows what order it's going to compile the modules in, because
plugins run per module).

But GHC's plugin interface is much nicer for rewriting test files, so
what we'll do here is:

1. Always register the plugin by adding `{\-# OPTIONS_GHC -fplugin=... #-\}` to
   the top of the file. The plugin will then inspect the file to see if it's
   a test file, and if so, convert the test module.

2. If the file is the main file, generate the main file and write a new file.
-}
module Main where

import qualified Data.Text.IO as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import Test.Tasty.AutoCollect (processFile)

main :: IO ()
main = do
  -- just to be extra sure we don't run into encoding issues
  setLocaleEncoding utf8

  getArgs >>= \case
    -- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#options-affecting-a-haskell-pre-processor
    [fp, input, output] -> Text.readFile input >>= processFile fp >>= Text.writeFile output
    _ -> error "The tasty-autocollect preprocessor does not accept any additional arguments."
