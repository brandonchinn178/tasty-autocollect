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

data Tree k v = Tree
  { fullPath :: [k]
  , value :: Maybe v
  , subTrees :: [Tree k v]
  }
  deriving (Show, Eq)

data TreeMap k v = TreeMap (Maybe v) (Map k (TreeMap k v))

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
  , value = Nothing
  , subTrees =
      [ Tree
        { fullPath = [A]
        , value = Nothing
        , subTrees =
            [ Tree
                { fullPath = [A, B]
                , value = Just ...
                , subTrees =
                    [ Tree
                        { fullPath = [A, B, C]
                        , value = Just ...
                        , subTrees = []
                        }
                    ]
                }
            , Tree
                { fullPath = [A, C]
                , value = Nothing
                , subTrees =
                    [ Tree
                        { fullPath = [A, C, D]
                        , value = Just ...
                        , subTrees = []
                        }
                    ]
                }
            ]
        }
    , Tree
        { fullPath = [Z]
        , value = Just ...
        , subTrees = []
        }
    ]
  }
@
-}
toTree :: Ord k => [([k], v)] -> Tree k v
toTree = fromTreeMap Seq.empty . foldr insertTreeMap emptyTreeMap
  where
    emptyTreeMap = TreeMap Nothing Map.empty

    insertTreeMap (ks, v) (TreeMap mVal children) =
      case ks of
        [] -> TreeMap (Just v) children
        k : ks' -> TreeMap mVal $ Map.alter (Just . insertTreeMap (ks', v) . fromMaybe emptyTreeMap) k children

    fromTreeMap path (TreeMap mVal children) =
      Tree
        { fullPath = toList path
        , value = mVal
        , subTrees = Map.elems $ Map.mapWithKey (fromTreeMap . (path |>)) children
        }
