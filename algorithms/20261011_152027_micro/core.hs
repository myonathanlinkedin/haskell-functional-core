module Core (
    Queue,
    empty,
    isEmpty,
    enqueue,
    dequeue,
    peek,
    fromList,
    toList
) where

-- | A purely functional queue with amortized O(1) operations.
--   Internally represented by two lists: front and rear.
--   The invariant is that if the front list is empty, the rear list is reversed
--   to become the new front.
data Queue a = Queue { front :: [a], rear :: [a] }
    deriving (Eq, Show)

-- | The empty queue.
empty :: Queue a
empty = Queue [] []

-- | Check whether the queue is empty.
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

-- | Ensure the invariant holds: if front is empty, reverse rear.
balance :: Queue a -> Queue a
balance (Queue [] r) = Queue (reverse r) []
balance q            = q

-- | Insert an element at the back of the queue.
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue f r) = balance $ Queue f (x:r)

-- | Remove the element at the front of the queue.
--   Returns Nothing if the queue is empty, otherwise Just (element, newQueue).
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue (x:xs) r) = Just (x, balance $ Queue xs r)
dequeue (Queue [] r) = -- front empty but rear non‑empty; balance will fix it
    case balance (Queue [] r) of
        Queue (y:ys) r' -> Just (y, balance $ Queue ys r')
        Queue [] _      -> Nothing  -- should never happen

-- | Look at the element at the front without removing it.
peek :: Queue a -> Maybe a
peek (Queue [] []) = Nothing
peek (Queue (x:_) _) = Just x
peek (Queue [] r) = case balance (Queue [] r) of
    Queue (y:_) _ -> Just y
    Queue [] _    -> Nothing

-- | Build a queue from a list (preserving order).
fromList :: [a] -> Queue a
fromList xs = Queue xs []

-- | Convert a queue back to a list (preserving order).
toList :: Queue a -> [a]
toList (Queue f r) = f ++ reverse r
