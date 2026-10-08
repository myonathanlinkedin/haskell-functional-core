module Main where

import Core
import Data.Array
import Data.Bits
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test for subsetConvolutionMin on n = 3
testSubsetConvolution :: IO ()
testSubsetConvolution = do
    let n = 3
        size = 1 `shiftL` n
        bounds = (0, size - 1)
        -- f[S] = popcount(S)
        fArr = listArray bounds [ popCount s | s <- range bounds ]
        -- g[S] = 2 * popcount(S)
        gArr = listArray bounds [ 2 * popCount s | s <- range bounds ]
        h = subsetConvolutionMin n fArr gArr
        -- brute-force expected values
        expected = listArray bounds [ brute s | s <- range bounds ]
        brute s = minimum [ fArr ! a + gArr ! (s `xor` a) | a <- subsets s ]
        subsets 0 = [0]
        subsets mask = go mask []
          where
            go 0 acc = 0 : acc
            go m acc = go ((m - 1) .&. mask) (m : acc)
    mapM_ (\s -> assert (h ! s == expected ! s)
                     ("subsetConvolutionMin mismatch at subset " ++ show s))
          (range bounds)

-- Test for joinOrderMinCost on a small example
testJoinOrder :: IO ()
testJoinOrder = do
    let sizes = [10, 20, 30]  -- three tables
        -- Brute-force enumeration of binary join trees (there are 2 possible orders)
        -- Order ((0 join 1) join 2):
        cost1 = 10*20 + (10+20)*30
        -- Order (0 join (1 join 2)):
        cost2 = 20*30 + 10*(20+30)
        expected = min cost1 cost2
        result = joinOrderMinCost sizes
    assert (result == expected) ("joinOrderMinCost expected " ++ show expected ++ ", got " ++ show result)

-- Entry point runs all tests
main :: IO ()
main = do
    putStrLn "Running tests..."
    testSubsetConvolution
    testJoinOrder
    putStrLn "All tests passed."
