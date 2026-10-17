module Main where

import Types
import Engine (secretSanta)
import System.Exit (exitFailure)

-- Simple assertion helper.
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn ("Assertion failed: " ++ msg) >> exitFailure

-- Pretty-print an assignment.
showAssignment :: Assignment -> String
showAssignment = unlines . map (\(g, r) -> g ++ " -> " ++ r)

-- Test cases.
testSimple :: IO ()
testSimple = do
    let participants = ["A","B","C"]
        exclusions = [] :: [Exclusion]
        expected = [("A","B"), ("B","C"), ("C","A")]
    case secretSanta participants exclusions of
        Just assign -> assert (assign == expected) "testSimple failed"
        Nothing -> assert False "testSimple returned Nothing"

testExclusion :: IO ()
testExclusion = do
    let participants = ["A","B","C"]
        exclusions = [("A","B")]
        expected = [("A","C"), ("B","A"), ("C","B")]
    case secretSanta participants exclusions of
        Just assign -> assert (assign == expected) "testExclusion failed"
        Nothing -> assert False "testExclusion returned Nothing"

testImpossible :: IO ()
testImpossible = do
    let participants = ["A","B"]
        exclusions = [("A","B"), ("B","A")]
    case secretSanta participants exclusions of
        Just _ -> assert False "testImpossible should have failed but succeeded"
        Nothing -> return ()

testSelfExclusionImplicit :: IO ()
testSelfExclusionImplicit = do
    let participants = ["A","B","C"]
        exclusions = [] :: [Exclusion]
    case secretSanta participants exclusions of
        Just assign -> 
            let selfGifts = any (\(g,r) -> g == r) assign
            in assert (not selfGifts) "Self-assignment detected"
        Nothing -> assert False "Unexpected Nothing in testSelfExclusionImplicit"

runAllTests :: IO ()
runAllTests = do
    testSimple
    testExclusion
    testImpossible
    testSelfExclusionImplicit
    putStrLn "All tests passed."

-- Demonstration with a larger set.
demo :: IO ()
demo = do
    let participants = ["Alice","Bob","Carol","Dave","Eve"]
        exclusions = [ ("Alice","Bob")
                     , ("Bob","Alice")
                     , ("Carol","Dave")
                     , ("Dave","Eve")
                     ]
    case secretSanta participants exclusions of
        Just assign -> do
            putStrLn "Secret Santa Assignment:"
            putStrLn (showAssignment assign)
        Nothing -> putStrLn "No valid assignment found."

main :: IO ()
main = do
    runAllTests
    demo
