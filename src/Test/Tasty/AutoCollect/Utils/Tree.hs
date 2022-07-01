module Test.Tasty.AutoCollect.Utils.Tree (
  Tree (..),
  toTree,
) where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

data Tree a = Tree
  { fullPath :: [a]
  , exists :: Bool
  , subTrees :: [Tree a]
  }
  deriving (Show, Eq)

data TreeMap a = TreeMap Bool (Map a (TreeMap a))

{- |
Convert the given list of values into a tree.

For example,
@
toTree [[A, B, C], [A, B], [A, C, D], [Z]]
@
would become
@
Tree
  { fullPath = []
  , exists = False
  , subTrees =
      [ Tree
        { fullPath = [A]
        , exists = False
        , subTrees =
            [ Tree
                { fullPath = [A, B]
                , exists = True
                , subTrees =
                    [ Tree
                        { fullPath = [A, B, C]
                        , exists = True
                        , subTrees = []
                        }
                    ]
                }
            , Tree
                { fullPath = [A, C]
                , exists = False
                , subTrees =
                    [ Tree
                        { fullPath = [A, C, D]
                        , exists = True
                        , subTrees = []
                        }
                    ]
                }
            ]
        }
    , Tree
        { fullPath = [Z]
        , exists = True
        , subTrees = []
        }
    ]
  }
@
-}
toTree :: Ord a => [[a]] -> Tree a
toTree = fromTreeMap Seq.empty . foldr insertTreeMap emptyTreeMap
  where
    emptyTreeMap = TreeMap False Map.empty

    insertTreeMap as (TreeMap e children) =
      case as of
        [] -> TreeMap True children
        a : as' -> TreeMap e $ Map.alter (Just . insertTreeMap as' . fromMaybe emptyTreeMap) a children

    fromTreeMap path (TreeMap e children) =
      Tree
        { fullPath = toList path
        , exists = e
        , subTrees = Map.elems $ Map.mapWithKey (fromTreeMap . (path |>)) children
        }
