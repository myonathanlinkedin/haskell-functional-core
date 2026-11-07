module Types (
    sort,
    Sortable(..)
) where

-- | A typeclass for sortable collections. Currently we provide an instance for lists.
class Sortable c where
    sort :: Ord a => c a -> c a

instance Sortable [] where
    sort = sortList

-- | Exposed pure sorting function for lists.
sortList :: Ord a => [a] -> [a]
sortList = sort
