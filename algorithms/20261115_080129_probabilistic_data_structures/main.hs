module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

-- Simple test harness ------------------------------------------------
type Test = (String, Bool)

runTests :: [Test] -> IO ()
runTests ts = do
    mapM_ report ts
    let failures = [n | (n, passed) <- ts, not passed]
    if null failures
        then putStrLn "All tests passed." >> exitSuccess
        else putStrLn ("Failed tests: " ++ unwords failures) >> exitFailure
  where
    report (n, True)  = putStrLn $ "[PASS] " ++ n
    report (n, False) = putStrLn $ "[FAIL] " ++ n

-- Unit tests ---------------------------------------------------------
testEmpty :: Test
testEmpty = ("emptyCMS dimensions", width cms == 10 && depth cms == 5)
  where cms = emptyCMS 10 5

testAddEstimate :: Test
testAddEstimate = ("add/estimate basic", estimate "apple" cms2 == 3)
  where
    cms0 = emptyCMS 20 4
    cms1 = add "apple" cms0
    cms2 = add "apple" $ add "banana" $ add "apple" cms1

testMerge :: Test
testMerge = ("merge correctness", estimate "x" merged == 5)
  where
    cmsA = foldr add (emptyCMS 15 3) (replicate 2 "x")
    cmsB = foldr add (emptyCMS 15 3) (replicate 3 "x")
    merged = merge cmsA cmsB

testDecay :: Test
testDecay = ("decay rounding", estimate "y" decayed == 1)
  where
    cms0 = foldr add (emptyCMS 8 2) (replicate 3 "y")
    decayed = decay 0.4 cms0   -- 3 * 0.4 = 1.2 -> floor 1

testHashDeterminism :: Test
testHashDeterminism = ("hash deterministic", h1 == h2)
  where
    cms = emptyCMS 12 2
    h1 = hashWithSeed "z" (head $ hashSeeds cms) 12
    h2 = hashWithSeed "z" (head $ hashSeeds cms) 12

testAll :: [Test]
testAll = [ testEmpty
          , testAddEstimate
          , testMerge
          , testDecay
          , testHashDeterminism
          ]

main :: IO ()
main = runTests testAll

--------------------------------------------------------------------
-- End of main.hs
--------------------------------------------------------------------
