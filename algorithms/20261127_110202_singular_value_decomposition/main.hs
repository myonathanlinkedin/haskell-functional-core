module Main where

import Core
import Prelude

epsilon :: Double
epsilon = 1e-6

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error ("Assertion failed: " ++ msg)

assertMatrix :: Matrix -> Matrix -> IO ()
assertMatrix a b = assert (approxEqualMatrix epsilon a b) "Matrices differ beyond tolerance"

testRankOneApprox :: IO ()
testRankOneApprox = do
    let a = [[3.0,1.0],[1.0,3.0]]
        (approx, ss, us, vs) = svdLowRank 1 a
        expected = [[2.0,2.0],[2.0,2.0]]
    assert (length ss == 1) "Expected one singular value"
    assertMatrix approx expected

testZeroMatrix :: IO ()
testZeroMatrix = do
    let a = [[0.0,0.0],[0.0,0.0]]
        (approx, ss, us, vs) = svdLowRank 2 a
        expected = [[0.0,0.0],[0.0,0.0]]
    assert (all (==0.0) ss) "All singular values should be zero"
    assertMatrix approx expected

testRectangular :: IO ()
testRectangular = do
    let a = [[1.0,2.0,3.0],[4.0,5.0,6.0]]
        (approx, ss, us, vs) = svdLowRank 2 a
        -- Reconstruct should equal original within tolerance
    assertMatrix approx a

main :: IO ()
main = do
    testRankOneApprox
    testZeroMatrix
    testRectangular
    putStrLn "All tests passed."
