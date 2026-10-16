module Main where

import Core
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
        then return ()
        else do
            putStrLn $ "FAIL: " ++ msg
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Actual:   " ++ show actual
            exitFailure

testHeap :: IO ()
testHeap = do
    let h0 = empty :: Heap (Int, Char)
        h1 = insert (5,'a') h0
        h2 = insert (3,'b') h1
        h3 = insert (7,'c') h2
    assertEqual "findMin after inserts" (Just (3,'b')) (findMin h3)
    let Just ((k1,_), h4) = deleteMin h3
    assertEqual "first deleteMin key" 3 k1
    let Just ((k2,_), h5) = deleteMin h4
    assertEqual "second deleteMin key" 5 k2
    let Just ((k3,_), _) = deleteMin h5
    assertEqual "third deleteMin key" 7 k3

testDijkstra :: IO ()
testDijkstra = do
    let g = Map.fromList [
                ('A', [('B',1), ('C',4)]),
                ('B', [('C',2), ('D',5)]),
                ('C', [('D',1)]),
                ('D', [])
              ]
        expected = Map.fromList [('A',0),('B',1),('C',3),('D',4)]
        result = dijkstra g 'A'
    assertEqual "Dijkstra distances" expected result

    let g2 = Map.fromList [
                (1, [(2,10)]),
                (2, [(3,10)]),
                (3, []),
                (4, [(5,1)])
              ]
        expected2 = Map.fromList [(1,0),(2,10),(3,20)]
        result2 = dijkstra g2 1
    assertEqual "Dijkstra with disconnected component" expected2 result2

main :: IO ()
main = do
    testHeap
    testDijkstra
    putStrLn "All tests passed."
