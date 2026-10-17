module Types where

import Data.Char (toLower)

-- Define data types for graph nodes and edges
data Node = Node {
    nodeValue :: Double, -- Value of the node
    nodeNeighbors :: [(Node, Double)] -- Neighbors and their corresponding temperatures
} deriving (Show)

data Edge = Edge {
    edgeNode :: Node, -- Source node
    edgeTarget :: Node -- Destination node
} deriving (Show)

-- Define data type for graph representation
data Graph = Graph {
    graphNodes :: [Node], -- List of nodes
    graphEdges :: [Edge] -- List of edges
} deriving (Show)

-- Define a function to convert a string to lowercase
toLowerCase :: String -> String
toLowerCase = map toLower
