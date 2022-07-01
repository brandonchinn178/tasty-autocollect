{- AUTOCOLLECT.TEST -}

module Test.Tasty.AutoCollect.Utils.TreeTest (
  {- AUTOCOLLECT.TEST.export -}
) where

import Test.Tasty.HUnit

import Test.Tasty.AutoCollect.Utils.Tree (Tree (..), toTree)

test_testCase :: Assertion
test_testCase "builds the correct tree" =
  toTree [["A", "B", "C"], ["A", "B"], ["A", "C", "D"], ["Z"]]
    @?= Tree
      { fullPath = []
      , exists = False
      , subTrees =
          [ Tree
              { fullPath = ["A"]
              , exists = False
              , subTrees =
                  [ Tree
                      { fullPath = ["A", "B"]
                      , exists = True
                      , subTrees =
                          [ Tree
                              { fullPath = ["A", "B", "C"]
                              , exists = True
                              , subTrees = []
                              }
                          ]
                      }
                  , Tree
                      { fullPath = ["A", "C"]
                      , exists = False
                      , subTrees =
                          [ Tree
                              { fullPath = ["A", "C", "D"]
                              , exists = True
                              , subTrees = []
                              }
                          ]
                      }
                  ]
              }
          , Tree
              { fullPath = ["Z"]
              , exists = True
              , subTrees = []
              }
          ]
      }
