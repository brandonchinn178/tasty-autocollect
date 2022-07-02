{- AUTOCOLLECT.TEST -}

module Test.Tasty.AutoCollectTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Test.Tasty.HUnit

test_testCase :: Assertion
test_testCase "smoke test" = pure ()

-- TODO: test parseModuleType case insensitive
-- TODO: test parseModuleType finds first comment
-- TODO: test AUTOCOLLECT.TEST.export inserts into correct spot in export list
-- TODO: test with/without OverloadedStrings, OverloadedLists, literal ints, literal floats
-- TODO: test error without tasty package in dependencies
-- TODO: test tree + strip-suffix with A.B.CTest and A.B.C.DTest matching
