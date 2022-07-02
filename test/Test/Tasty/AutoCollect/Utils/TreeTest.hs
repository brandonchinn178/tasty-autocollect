{- AUTOCOLLECT.TEST -}

module Test.Tasty.AutoCollect.Utils.TreeTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Test.Tasty.HUnit

import Test.Tasty.AutoCollect.Utils.Tree (Tree (..), toTree)

test_testCase :: Assertion
test_testCase "builds the correct tree" =
  toTree (zip [["A", "B", "C"], ["A", "B"], ["A", "C", "D"], ["Z"]] [1 :: Int ..])
    @?= Tree
      { fullPath = []
      , value = Nothing
      , subTrees =
          [ Tree
              { fullPath = ["A"]
              , value = Nothing
              , subTrees =
                  [ Tree
                      { fullPath = ["A", "B"]
                      , value = Just 2
                      , subTrees =
                          [ Tree
                              { fullPath = ["A", "B", "C"]
                              , value = Just 1
                              , subTrees = []
                              }
                          ]
                      }
                  , Tree
                      { fullPath = ["A", "C"]
                      , value = Nothing
                      , subTrees =
                          [ Tree
                              { fullPath = ["A", "C", "D"]
                              , value = Just 3
                              , subTrees = []
                              }
                          ]
                      }
                  ]
              }
          , Tree
              { fullPath = ["Z"]
              , value = Just 4
              , subTrees = []
              }
          ]
      }
