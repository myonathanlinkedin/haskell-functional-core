module Core where

import System.Random
import Data.List (sort, sortBy)
import Data.Ord (comparing)

-- | Represents a node in the graph
data Node = Node { nodeId :: Int, x :: Double, y :: Double }
  deriving (Show, Eq)

-- | Represents an edge in the graph
data Edge = Edge { fromNode :: Int, toNode :: Int, weight :: Double }
  deriving (Show, Eq)

-- | Represents a graph
data Graph = Graph { nodes :: [Node], edges :: [Edge] }
  deriving (Show, Eq)

-- | Represents a solution (permutation of nodes)
type Solution = [Int]

-- | Represents the state of the simulated annealing process
data SAState = SAState
  { currentSolution :: Solution
  , currentEnergy :: Double
  , bestSolution :: Solution
  , bestEnergy :: Double
  , temperature :: Double
  , iteration :: Int
  } deriving (Show, Eq)

-- | Configuration for simulated annealing
data SAConfig = SAConfig
  { initialTemp :: Double
  , coolingRate :: Double
  , maxIterations :: Int
  , seed :: Int
  } deriving (Show, Eq)

-- | Calculate the total weight of a path given a solution and graph
calculateEnergy :: Graph -> Solution -> Double
calculateEnergy graph solution =
  sum [ weight e | e <- edges graph,
                let (from, to) = (fromNode e, toNode e),
                let idxFrom = elemIndex from solution,
                let idxTo = elemIndex to solution,
                isJust idxFrom,
                isJust idxTo,
                let iFrom = fromJust idxFrom,
                let iTo = fromJust idxTo,
                abs (iFrom - iTo) == 1 ]

-- | Generate a random neighbor solution by swapping two elements
generateNeighbor :: Solution -> StdGen -> (Solution, StdGen)
generateNeighbor solution gen =
  let (i, gen1) = randomR (0, length solution - 1) gen
      (j, gen2) = randomR (0, length solution - 1) gen1
      newSolution = swap i j solution
  in (newSolution, gen2)

-- | Swap two elements in a list
swap :: Int -> Int -> [a] -> [a]
swap i j xs
  | i == j = xs
  | otherwise =
      let (before, rest) = splitAt i xs
          (middle, after) = splitAt (j - i) rest
          (x, rest1) = (head middle, tail middle)
          (y, rest2) = (head after, tail after)
      in before ++ [y] ++ middle ++ [x] ++ rest2

-- | Accept a new solution based on the Metropolis criterion
acceptSolution :: Double -> Double -> Double -> StdGen -> (Bool, StdGen)
acceptSolution currentEnergy newEnergy temperature gen =
  if newEnergy <= currentEnergy
    then (True, gen)
    else
      let delta = newEnergy - currentEnergy
          probability = exp (-delta / temperature)
          (rand, gen') = random gen
      in (rand < probability, gen')

-- | Perform one iteration of simulated annealing
saIteration :: Graph -> SAState -> StdGen -> (SAState, StdGen)
saIteration graph state gen =
  let currentSol = currentSolution state
      currentEn = currentEnergy state
      (neighborSol, gen1) = generateNeighbor currentSol gen
      neighborEn = calculateEnergy graph neighborSol
      (accepted, gen2) = acceptSolution currentEn neighborEn (temperature state) gen1
      newSol = if accepted then neighborSol else currentSol
      newEn = if accepted then neighborEn else currentEn
      newBestSol = if newEn < bestEnergy state then newSol else bestSolution state
      newBestEn = if newEn < bestEnergy state then newEn else bestEnergy state
      newTemp = temperature state * coolingRate (configFromState state)
      newState = SAState newSol newEn newBestSol newBestEn newTemp (iteration state + 1)
  in (newState, gen2)

-- | Extract configuration from state (placeholder for actual config)
configFromState :: SAState -> Double
configFromState _ = 0.995

-- | Initialize the simulated annealing state
initializeSA :: Graph -> SAConfig -> (SAState, StdGen)
initializeSA graph config =
  let nodeIds = map nodeId (nodes graph)
      initialSol = nodeIds
      initialEn = calculateEnergy graph initialSol
      initialGen = mkStdGen (seed config)
      initialState = SAState initialSol initialEn initialSol initialEn (initialTemp config) 0
  in (initialState, initialGen)

-- | Run simulated annealing optimization
runSimulatedAnnealing :: Graph -> SAConfig -> (Solution, Double)
runSimulatedAnnealing graph config =
  let (initialState, initialGen) = initializeSA graph config
      finalState = runSAIterations graph initialState initialGen (maxIterations config)
  in (bestSolution finalState, bestEnergy finalState)

-- | Recursively run SA iterations
runSAIterations :: Graph -> SAState -> StdGen -> Int -> SAState
runSAIterations graph state gen remaining
  | remaining <= 0 = state
  | temperature state < 1e-10 = state
  | otherwise =
      let (newState, newGen) = saIteration graph state gen
      in runSAIterations graph newState newGen (remaining - 1)

-- | Create a sample graph for testing
createSampleGraph :: Graph
createSampleGraph = Graph
  { nodes = [Node 0 0.0 0.0, Node 1 1.0 0.0, Node 2 1.0 1.0, Node 3 0.0 1.0, Node 4 0.5 0.5]
  , edges = [Edge 0 1 1.0, Edge 1 2 1.0, Edge 2 3 1.0, Edge 3 0 1.0, Edge 0 4 0.7, Edge 1 4 0.7, Edge 2 4 0.7, Edge 3 4 0.7]
  }
