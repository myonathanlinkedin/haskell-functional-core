module Main where

import Core (RBTree, empty, insert, member, validate)

-- Verify that after each insertion the tree remains valid
testIncremental :: (Ord a) => [a] -> Bool
testIncremental = go empty
  where
    go _ [] = True
    go t (x:xs) = let t' = insert x t
                  in validate t' && go t' xs

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

main :: IO ()
main = do
    let values = [10,20,30,15,5,25,2,8,12,18]
        tree   = foldr insert empty values

    -- Global validation after all insertions
    assert (validate tree) "Tree failed global validation"

    -- Incremental validation
    assert (testIncremental values) "Incremental validation failed"

    -- Membership tests for inserted elements
    mapM_ (\x -> assert (member x tree) ("Missing element " ++ show x)) values

    -- Non-membership tests
    mapM_ (\x -> assert (not (member x tree)) ("Unexpected element " ++ show x))
          [0, 100, 17]

    putStrLn "All red-black tree tests passed."
