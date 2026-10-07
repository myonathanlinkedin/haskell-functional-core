module Types (BST(..)) where

data BST a = Empty
           | Node (BST a) a (BST a)
           deriving (Show, Eq)
