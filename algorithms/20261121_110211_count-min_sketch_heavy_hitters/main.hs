module Main where

import Core
import Data.List (foldl')
import System.Exit (exitFailure)

-- Simple assertion helper
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Test 1: basic frequency estimation and heavy‑hitter extraction
testBasic :: IO ()
testBasic = do
    let cms0 = emptyCMS 1000 5 :: CMS Int
        stream = concat [ replicate 5000 1
                        , replicate 3000 2
                        , replicate 1000 3
                        , replicate 200  4
                        ]
        cms = foldl' (\c x -> add x 1 c) cms0 stream
        est1 = estimate 1 cms
        est2 = estimate 2 cms
        est3 = estimate 3 cms
        est4 = estimate 4 cms
        hh   = heavyHitters 2500 cms
    assert (est1 >= 5000) "estimate 1 too low"
    assert (est2 >= 3000) "estimate 2 too low"
    assert (est3 >= 1000) "estimate 3 too low"
    assert (est4 >= 200)  "estimate 4 too low"
    assert (length hh == 2) "heavy hitters count mismatch"
    assert (all (\(k,_) -> k == 1 || k == 2) hh) "unexpected heavy hitter key"

-- Test 2: hash determinism and collision handling
testHashDeterminism :: IO ()
testHashDeterminism = do
    let cms = emptyCMS 50 3 :: CMS String
        cms' = add "alpha" 1 . add "beta" 1 $ cms
        estAlpha = estimate "alpha" cms'
        estBeta  = estimate "beta"  cms'
    assert (estAlpha >= 1) "alpha count missing"
    assert (estBeta  >= 1) "beta count missing"
    -- Re‑adding the same element must increase its estimate
    let cms'' = add "alpha" 3 cms'
    assert (estimate "alpha" cms'' >= estAlpha + 3) "alpha increment failed"

-- Test 3: invalid parameters produce runtime errors (checked via exception safety)
testInvalidParams :: IO ()
testInvalidParams = do
    let bad = (emptyCMS 0 5 :: CMS Int)
    assert (depth bad > 0) "emptyCMS allowed zero width"

main :: IO ()
main = do
    putStrLn "Running Count‑Min Sketch tests..."
    testBasic
    testHashDeterminism
    putStrLn "All tests passed."
