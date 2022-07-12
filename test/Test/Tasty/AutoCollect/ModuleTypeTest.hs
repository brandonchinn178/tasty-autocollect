{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.AutoCollect.ModuleTypeTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import qualified Data.Text as Text
import Test.Predicates
import Test.Predicates.HUnit
import Test.Predicates.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Tasty.AutoCollect.Config
import Test.Tasty.AutoCollect.ModuleType
import TestUtils.QuickCheck

test = testCase "parseModuleType finds first comment" $ do
  parseModuleType (Text.unlines [test, main]) @?= Just ModuleTest
  parseModuleType (Text.unlines [main, test]) @?~ just ($(qADT 'ModuleMain) anything)
  where
    main = "{- AUTOCOLLECT.MAIN -}"
    test = "{- AUTOCOLLECT.TEST -}"

test = testProperty "parseModuleType parses MAIN case-insensitive" $
  forAll (genMixedCase "{- AUTOCOLLECT.MAIN -}") $ \main ->
    parseModuleType main `satisfies` just ($(qADT 'ModuleMain) anything)

test = testProperty "parseModuleType parses TEST case-insensitive" $
  forAll (genMixedCase "{- AUTOCOLLECT.TEST -}") $ \test ->
    parseModuleType test === Just ModuleTest

test = testCase "parseModuleType parses configuration for main modules" $ do
  parseModuleType "{- AUTOCOLLECT.MAIN suite_name = foo -}"
    @?~ just ($(qADT 'ModuleMain) $ cfgSuiteName `with` eq (Just "foo"))
  parseModuleType "{- AUTOCOLLECT.MAIN\nsuite_name = foo\n-}"
    @?~ just ($(qADT 'ModuleMain) $ cfgSuiteName `with` eq (Just "foo"))
