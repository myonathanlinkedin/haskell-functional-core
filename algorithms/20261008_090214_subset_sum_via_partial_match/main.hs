module Main where

import Types (IntSet)
import Engine (subsetSumExists)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test cases: (input set, target sum, expected result)
testCases :: [(IntSet, Int, Bool)]
testCases =
  [ ([], 0, True)                     -- empty set, zero target
  , ([], 5, False)                    -- empty set, non-zero target
  , ([1, 2, 3, 4, 5], 9, True)        -- 4+5 or 2+3+4
  , ([1, 2, 3, 4, 5], 20, False)      -- sum exceeds total
  , ([3, 34, 4, 12, 5, 2], 9, True)   -- classic example
  , ([3, 34, 4, 12, 5, 2], 30, True)  -- 12+5+4+3+2+4? actually 12+5+4+3+2+4? but there is 12+5+4+3+2=26; need correct: 12+5+4+3+2+4? duplicate not allowed. Let's use known: 34+ -? We'll set expected False.
  , ([3, 34, 4, 12, 5, 2], 30, False)
  , ([-7, -3, -2, 5, 8], 0, True)    -- -7 + -3 + -2 + 5 + 8 = 1? Actually -7-3-2+5+8=1, not zero. Use -7 + -3 + 5 + 5? Not present. Let's use -7 + -3 + 5 + 5? Not. We'll set target -2 ( -7 +5 = -2 )
  , ([-7, -3, -2, 5, 8], -2, True)
  ]

runTests :: IO ()
runTests = mapM_ run testCases
  where
    run (xs, tgt, expected) =
      let result = subsetSumExists xs tgt
          msg = "subsetSumExists " ++ show xs ++ " " ++ show tgt ++ " == " ++ show expected
       in assert (result == expected) msg

main :: IO ()
main = do
  runTests
  putStrLn "All tests passed."
  -- Demo
  let demoSet = [1, 3, 9, 2]
      demoTarget = 8
  putStrLn $ "Demo: does " ++ show demoSet ++ " have a subset summing to " ++ show demoTarget ++ "? " ++ show (subsetSumExists demoSet demoTarget)
