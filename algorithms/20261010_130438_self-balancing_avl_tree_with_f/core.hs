module Core (AVL(..), empty, insert, delete, member, fromList, toList, isBalanced) where

data AVL a = Empty
           | Node { value :: a
                  , left  :: AVL a
                  , right :: AVL a
                  , h     :: Int
                  } deriving (Show, Eq)

empty :: AVL a
empty = Empty

height :: AVL a -> Int
height Empty                = 0
height (Node _ _ _ h')      = h'

mkNode :: a -> AVL a -> AVL a -> AVL a
mkNode v l r = Node v l r (1 + max (height l) (height r))

balanceFactor :: AVL a -> Int
balanceFactor Empty          = 0
balanceFactor (Node _ l r _) = height l - height r

rotateRight :: AVL a -> AVL a
rotateRight (Node z (Node y t1 t2 _) t3 _) = mkNode y t1 (mkNode z t2 t3)
rotateRight t                              = t

rotateLeft :: AVL a -> AVL a
rotateLeft (Node z t1 (Node y t2 t3 _) _) = mkNode y (mkNode z t1 t2) t3
rotateLeft t                              = t

balance :: AVL a -> AVL a
balance t@(Node v l r _)
  | bf > 1 && balanceFactor l >= 0 = rotateRight t
  | bf > 1                         = rotateRight (mkNode v (rotateLeft l) r)
  | bf < -1 && balanceFactor r <= 0 = rotateLeft t
  | bf < -1                         = rotateLeft (mkNode v l (rotateRight r))
  | otherwise                       = mkNode v l r
  where bf = balanceFactor t
balance t = t

insert :: Ord a => a -> AVL a -> AVL a
insert x Empty = Node x Empty Empty 1
insert x t@(Node v l r _)
  | x < v     = balance (mkNode v (insert x l) r)
  | x > v     = balance (mkNode v l (insert x r))
  | otherwise = t

member :: Ord a => a -> AVL a -> Bool
member _ Empty = False
member x (Node v l r _)
  | x < v     = member x l
  | x > v     = member x r
  | otherwise = True

findMin :: AVL a -> a
findMin (Node v Empty _ _) = v
findMin (Node _ l _ _)     = findMin l
findMin Empty              = error "findMin on empty tree"

deleteMin :: AVL a -> AVL a
deleteMin (Node _ Empty r _) = r
deleteMin (Node v l r _)     = balance (mkNode v (deleteMin l) r)
deleteMin Empty              = Empty

delete :: Ord a => a -> AVL a -> AVL a
delete _ Empty = Empty
delete x (Node v l r _)
  | x < v     = balance (mkNode v (delete x l) r)
  | x > v     = balance (mkNode v l (delete x r))
  | otherwise = case (l, r) of
      (Empty, _) -> r
      (_, Empty) -> l
      _          -> let m  = findMin r
                        r' = deleteMin r
                    in balance (mkNode m l r')

fromList :: Ord a => [a] -> AVL a
fromList = foldr insert Empty

toList :: AVL a -> [a]
toList Empty                = []
toList (Node v l r _) = toList l ++ [v] ++ toList r

isBalanced :: AVL a -> Bool
isBalanced Empty = True
isBalanced (Node _ l r _) =
  abs (height l - height r) <= 1 && isBalanced l && isBalanced r
