module Core where

import Data.Array
import Data.Bits
import Data.List
import qualified Data.Map.Strict as Map

type Graph = Array Int [Int]

data TreeNode = TreeNode
  { bag      :: [Int]
  , children :: [TreeNode]
  }

-- Build a graph from number of vertices and edge list
buildGraph :: Int -> [(Int,Int)] -> Graph
buildGraph n edges = accumArray (flip (:)) [] (0,n-1) allEdges
  where
    allEdges = edges ++ map swap edges
    swap (u,v) = (v,u)

-- Check if a subset (bitmask) of a bag is an independent set within the bag
isIndependentBag :: [Int] -> Graph -> Int -> Bool
isIndependentBag bag g mask = all checkPair pairs
  where
    vertices = [v | (i,v) <- zip [0..] bag, testBit mask i]
    pairs    = [(v,w) | (i,v) <- zip [0..] bag
                      , (j,w) <- zip [0..] bag
                      , i < j
                      , testBit mask i
                      , testBit mask j]
    checkPair (v,w) = not (w `elem` g ! v)

-- Generate all independent subsets of a bag
independentSubsets :: [Int] -> Graph -> [Int]
independentSubsets bag g = filter (isIndependentBag bag g) masks
  where
    size  = length bag
    masks = [0 .. (1 `shiftL` size) - 1]

-- Compute DP for a tree node
dpNode :: Graph -> TreeNode -> Map.Map Int Integer
dpNode g node = nodeDP
  where
    bagNodes = bag node
    size     = length bagNodes
    posMapParent = Map.fromList $ zip bagNodes [0..]
    childDPs = map (dpNode g) (children node)
    childContribs = map (childContribution posMapParent bagNodes) (zip (children node) childDPs)

    nodeDP = Map.fromList
      [ (mask, product [ Map.findWithDefault 1 (intersectionMask posMapParent mask inter) childContrib
                       | (inter, childContrib) <- childContribs ])
      | mask <- independentSubsets bagNodes g
      ]

-- For a child, map its DP entries to parent intersection masks
childContribution :: Map.Map Int Int -> [Int] -> (TreeNode, Map.Map Int Integer) -> ([Int], Map.Map Int Integer)
childContribution posMapParent bagNodes (child, childDP) = (inter, contribMap)
  where
    inter = bagNodes `intersect` bag child
    posMapChild = Map.fromList $ zip (bag child) [0..]
    contribMap = Map.fromListWith (+)
      [ (parentMask, count)
      | (childMask, count) <- Map.toList childDP
      , let parentMask = foldl' setBit 0
                      [ posMapParent Map.! v
                      | v <- inter
                      , testBit childMask (posMapChild Map.! v)
                      ]
      ]

-- Compute intersection mask of a bag subset with a set of vertices
intersectionMask :: Map.Map Int Int -> Int -> [Int] -> Int
intersectionMask posMapParent mask inter = foldl' setBit 0
  [ posMapParent Map.! v
  | v <- inter
  , testBit mask (posMapParent Map.! v)
  ]

-- Count independent sets in a graph using a tree decomposition
countIndependentSets :: Graph -> TreeNode -> Integer
countIndependentSets g root = sum $ Map.elems $ dpNode g root
