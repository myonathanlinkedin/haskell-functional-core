module Core (Color(..), RBTree(..), empty, insert, member, validate) where

-- Color of a node: Red or Black
data Color = R | B deriving (Eq, Show)

-- Red-Black Tree definition
data RBTree a = E                         -- Empty leaf (black)
              | T Color (RBTree a) a (RBTree a)  -- Node with color, left, value, right
              deriving (Show)

empty :: RBTree a
empty = E

-- Helper: check if a node is red
isRed :: RBTree a -> Bool
isRed (T R _ _ _) = True
isRed _           = False

-- Public membership test
member :: Ord a => a -> RBTree a -> Bool
member _ E = False
member x (T _ l y r)
  | x < y     = member x l
  | x > y     = member x r
  | otherwise = True

-- Insert a value, preserving red-black invariants
insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = makeBlack $ ins t
  where
    ins E = T R E x E
    ins (T color a y b)
      | x < y     = balance color (ins a) y b
      | x > y     = balance color a y (ins b)
      | otherwise = T color a y b   -- duplicate: no change
    makeBlack (T _ a y b) = T B a y b
    makeBlack E           = E   -- never reached

-- Balancing step after insertion (standard cases)
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color l v r = T color l v r

-- Validate red-black invariants:
-- 1. Root is black.
-- 2. No red node has a red child.
-- 3. All paths have the same black-height.
validate :: Ord a => RBTree a -> Bool
validate t = case t of
    E -> True
    T color _ _ _ -> color == B && check t /= Nothing
  where
    -- Returns Just black-height if subtree is valid, Nothing otherwise
    check :: RBTree a -> Maybe Int
    check E = Just 1
    check (T col l _ r) = do
        lh <- check l
        rh <- check r
        if lh /= rh then Nothing else
            if col == R && (isRed l || isRed r) then Nothing else
                Just (lh + if col == B then 1 else 0)
