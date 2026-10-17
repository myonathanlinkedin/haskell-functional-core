module Core (Graph, anneal) where

import System.Random (RandomGen, randomR)

type Graph = [[Double]]

-- Swap two elements in a list (purely functional)
swap :: Int -> Int -> [a] -> [a]
swap i j xs
  | i == j    = xs
  | otherwise = let vi = xs !! i
                    vj = xs !! j
                    replace k v ys = take k ys ++ [v] ++ drop (k + 1) ys
                in replace i vj (replace j vi xs)

-- Total length of a Hamiltonian cycle defined by a permutation of vertices
cost :: [Int] -> Graph -> Double
cost path g = sum $ zipWith edgeDist path (tail path ++ [head path])
  where
    edgeDist a b = g !! a !! b

-- Produce a neighbour by swapping two random positions
randomNeighbor :: RandomGen g => [Int] -> g -> ([Int], g)
randomNeighbor sol gen =
  let n = length sol
      (i, gen1) = randomR (0, n - 1) gen
      (j, gen2) = randomR (0, n - 1) gen1
      (i', j') = if i < j then (i, j) else (j, i)
  in (swap i' j' sol, gen2)

-- Uniform random Double in [0,1)
randomProb :: RandomGen g => g -> (Double, g)
randomProb = randomR (0.0, 1.0)

-- Simulated Annealing core routine
anneal :: RandomGen g
       => Graph      -- ^ Complete weighted graph (adjacency matrix)
       -> Double     -- ^ Initial temperature
       -> Double     -- ^ Final temperature
       -> Double     -- ^ Cooling factor (alpha, 0 < alpha < 1)
       -> Int        -- ^ Iterations per temperature level
       -> g          -- ^ Random generator (seeded externally)
       -> ([Int], Double)   -- ^ Best permutation found and its cost
anneal graph initTemp finalTemp alpha iterPerTemp gen0 = go initTemp initSol initCost gen0 bestSol bestCost
  where
    n = length graph
    initSol = [0 .. n - 1]
    initCost = cost initSol graph
    bestSol = initSol
    bestCost = initCost

    go :: Double -> [Int] -> Double -> g -> [Int] -> Double -> ([Int], Double)
    go temp curSol curCost gen bestS bestC
      | temp <= finalTemp = (bestS, bestC)
      | otherwise =
          let (gen', curSol', curCost', bestS', bestC') = iterateTemp iterPerTemp temp curSol curCost bestS bestC gen
              temp' = temp * alpha
          in go temp' curSol' curCost' gen' bestS' bestC'

    iterateTemp :: Int -> Double -> [Int] -> Double -> [Int] -> Double -> g
                 -> (g, [Int], Double, [Int], Double)
    iterateTemp 0 _ curSol curCost bestS bestC g = (g, curSol, curCost, bestS, bestC)
    iterateTemp k temp curSol curCost bestS bestC g =
      let (neighborSol, g1) = randomNeighbor curSol g
          neighborCost = cost neighborSol graph
          delta = neighborCost - curCost
          (prob, g2) = randomProb g1
          accept = delta < 0 || exp (-delta / temp) > prob
          (newCurSol, newCurCost) = if accept then (neighborSol, neighborCost) else (curSol, curCost)
          (newBestS, newBestC) = if neighborCost < bestC then (neighborSol, neighborCost) else (bestS, bestC)
      in iterateTemp (k - 1) temp newCurSol newCurCost newBestS newBestC g2
