module Engine where

import Types
import System.Random (RandomGen, randomR)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Monad (foldM)

-- | Parameters controlling the annealing schedule.
data SAParams = SAParams
  { initialTemp   :: Double   -- ^ Starting temperature (>0)
  , finalTemp     :: Double   -- ^ Temperature at which to stop (>=0)
  , alpha         :: Double   -- ^ Multiplicative cooling factor (0 < alpha < 1)
  , iterPerTemp   :: Int      -- ^ Number of candidate moves per temperature
  } deriving (Show, Eq)

-- | Core simulated annealing algorithm.
--   Returns the best tour found, its cost, and the final random generator.
simulatedAnnealing
  :: RandomGen g
  => Graph      -- ^ Problem instance
  -> Tour       -- ^ Starting tour (must contain all vertices exactly once)
  -> SAParams   -- ^ Annealing schedule parameters
  -> g          -- ^ Initial random generator
  -> (Tour, Double, g)   -- ^ (Best tour, its cost, final generator)
simulatedAnnealing g startTour params gen0 = go (initialTemp params) startTour startCost startTour startCost gen0
  where
    startCost = tourCost g startTour

    -- Main loop over temperatures
    go :: Double -> Tour -> Double -> Tour -> Double -> g -> (Tour, Double, g)
    go temp curTour curCost bestTour bestCost gen
      | temp <= finalTemp params = (bestTour, bestCost, gen)
      | otherwise =
          let (newTour, newCost, gen') = iterateTemp temp curTour curCost gen (iterPerTemp params)
              (newBestTour, newBestCost) = if newCost < bestCost
                                            then (newTour, newCost)
                                            else (bestTour, bestCost)
          in go (temp * alpha params) newTour newCost newBestTour newBestCost gen'

    -- Perform a fixed number of candidate moves at a given temperature
    iterateTemp :: Double -> Tour -> Double -> g -> Int -> (Tour, Double, g)
    iterateTemp _ tour cost gen 0 = (tour, cost, gen)
    iterateTemp temp tour cost gen n =
      let (i, gen1) = randomR (0, length tour - 1) gen
          (j, gen2) = randomR (0, length tour - 1) gen1
          candTour = neighborTour tour i j
          candCost = tourCost g candTour
          delta = candCost - cost
          acceptProb = if delta <= 0
                         then 1.0
                         else exp ( - delta / temp )
          (r, gen3) = randomR (0.0, 1.0) gen2
          (nextTour, nextCost) = if r < acceptProb
                                   then (candTour, candCost)
                                   else (tour, cost)
      in iterateTemp temp nextTour nextCost gen3 (n-1)
