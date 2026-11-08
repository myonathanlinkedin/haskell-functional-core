module Core (
    Message,
    AVLTree,
    empty,
    insert,
    rangeQuery,
    deleteBefore
) where

import Data.List (foldl')

-- | A message consists of a timestamp (Int) and an arbitrary payload (String).
type Message = (Int, String)

-- | Pure AVL tree indexed by the timestamp of the message.
data AVLTree
    = Empty
    | Node {
        left   :: AVLTree,
        value  :: Message,
        right  :: AVLTree,
        height :: Int
    } deriving (Show, Eq)

-- | The empty tree.
empty :: AVLTree
empty = Empty

-- Helper: height of a node.
nodeHeight :: AVLTree -> Int
nodeHeight Empty = 0
nodeHeight n     = height n

-- Helper: create a node with correct height.
mkNode :: AVLTree -> Message -> AVLTree -> AVLTree
mkNode l v r = Node l v r h
  where
    h = 1 + max (nodeHeight l) (nodeHeight r)

-- Balance factor = height(left) - height(right)
balanceFactor :: AVLTree -> Int
balanceFactor Empty = 0
balanceFactor n     = nodeHeight (left n) - nodeHeight (right n)

-- Rotations ---------------------------------------------------------

rotateRight :: AVLTree -> AVLTree
rotateRight (Node (Node a x b _) y c _) = mkNode a x (mkNode b y c)
rotateRight t = t  -- should never happen for a balanced tree

rotateLeft :: AVLTree -> AVLTree
rotateLeft (Node a x (Node b y c _) _) = mkNode (mkNode a x b) y c
rotateLeft t = t  -- should never happen for a balanced tree

-- Rebalance a node after insertion/deletion.
balance :: AVLTree -> AVLTree
balance t@(Node l v r _)
    | bf > 1 && balanceFactor l >= 0 = rotateRight t
    | bf > 1 && balanceFactor l < 0  = rotateRight (mkNode (rotateLeft l) v r)
    | bf < -1 && balanceFactor r <= 0 = rotateLeft t
    | bf < -1 && balanceFactor r > 0  = rotateLeft (mkNode l v (rotateRight r))
    | otherwise = mkNode l v r
  where
    bf = balanceFactor t
balance t = t

-- Insertion ---------------------------------------------------------

-- Insert a message, ordering by timestamp (first component).
insert :: AVLTree -> Message -> AVLTree
insert Empty msg = mkNode Empty msg Empty
insert n@(Node l v r _) msg
    | ts < tsV  = balance $ mkNode (insert l msg) v r
    | ts > tsV  = balance $ mkNode l v (insert r msg)
    | otherwise = n  -- duplicate timestamps are ignored
  where
    ts   = fst msg
    tsV  = fst v

-- Range query -------------------------------------------------------

-- Collect messages whose timestamps lie in [low,high] (inclusive) in ascending order.
rangeQuery :: AVLTree -> Int -> Int -> [Message]
rangeQuery Empty _ _ = []
rangeQuery (Node l v r _) low high
    | ts < low  = rangeQuery r low high
    | ts > high = rangeQuery l low high
    | otherwise = rangeQuery l low high ++ [v] ++ rangeQuery r low high
  where
    ts = fst v

-- Delete all messages with timestamp < bound.
deleteBefore :: AVLTree -> Int -> AVLTree
deleteBefore Empty _ = Empty
deleteBefore (Node l v r _) bound
    | ts < bound = deleteBefore r bound  -- left subtree is entirely < bound
    | otherwise  = balance $ mkNode (deleteBefore l bound) v (deleteBefore r bound)
  where
    ts = fst v
