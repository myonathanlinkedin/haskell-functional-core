module Engine (insert, delete, find, inorder) where

import Types (BST(..))

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left v right)
    | x < v     = Node (insert x left) v right
    | x > v     = Node left v (insert x right)
    | otherwise = Node left v right  -- ignore duplicates

find :: Ord a => a -> BST a -> Bool
find _ Empty = False
find x (Node left v right)
    | x < v     = find x left
    | x > v     = find x right
    | otherwise = True

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node left v right) = inorder left ++ [v] ++ inorder right

delete :: Ord a => a -> BST a -> BST a
delete _ Empty = Empty
delete x (Node left v right)
    | x < v = Node (delete x left) v right
    | x > v = Node left v (delete x right)
    | otherwise = deleteNode (Node left v right)

deleteNode :: BST a -> BST a
deleteNode (Node Empty _ right) = right
deleteNode (Node left _ Empty)  = left
deleteNode (Node left _ right)  =
    let (predVal, newLeft) = extractMax left
    in Node newLeft predVal right

extractMax :: BST a -> (a, BST a)
extractMax (Node left v Empty) = (v, left)
extractMax (Node left v right) =
    let (maxVal, newRight) = extractMax right
    in (maxVal, Node left v newRight)
