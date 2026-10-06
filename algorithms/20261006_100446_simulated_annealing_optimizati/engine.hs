module Engine where

import Data.List (foldl')
import Data.Char (toLower)

-- Define a function to convert a string to lowercase
toLowerCase :: String -> String
toLowerCase = toLower

-- Define a function to calculate the temperature of a node
calculateTemperature :: Node -> Double
calculateTemperature node = nodeValue node / (fromIntegral $ length $ graphNodes $ graphFromString graph)

-- Define a function to calculate the energy of a graph
calculateEnergy :: Graph -> Double
calculateEnergy graph = sum $ map (\node -> nodeValue node * calculateTemperature node) $ graphNodes graph

-- Define a function to calculate the cooling rate
calculateCoolingRate :: Double -> Double -> Double
calculateCoolingRate oldTemp newTemp = (1 / (1 + (newTemp / oldTemp)) ^ 2)

-- Define a function to perform a single simulated annealing step
anneal :: Graph -> Double -> Node -> IO ()
anneal graph temp targetNode = do
    let newTemp = oldTemp * calculateCoolingRate oldTemp temp
    let newNode = graphNodes graph !! targetNode
    let newValue = calculateTemperature newNode * newNodeValue newNode
    let newGraph = map (\node -> if node == targetNode then newNode else node) $ graphNodes graph
    let newEdges = map (\edge -> Edge (graphNodes graph !! edgeSrc) (graphNodes graph !! edgeDst) edgeValue) $ graphEdges graph
    let newGraph' = Graph newGraph newEdges
    let newValue' = calculateEnergy newGraph
    let newTemp' = newTemp * calculateCoolingRate newTemp newTemp'
    let newGraph'' = Graph newGraph newEdges
    let newValue'' = calculateEnergy newGraph''
    let newTemp'' = newTemp * calculateCoolingRate newTemp newTemp''

    putStrLn $ "Temperature: " ++ show newTemp''
    putStrLn $ "Value: " ++ show newValue''
    putStrLn $ "Value of target node: " ++ show newValue'
    putStrLn $ "Edges in new graph: " ++ show newEdges
    putStrLn $ "Graph after mutation: " ++ show newGraph''
    putStrLn $ "Edges in mutated graph: " ++ show newEdges'
    putStrLn $ "Graph after cooling: " ++ show newGraph'''
    putStrLn $ "Edges in mutated graph: " ++ show newEdges'''
