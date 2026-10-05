module Core where

import System.Random (StdGen, mkStdGen, randomR)
import Data.List (mapAccumL, maximumBy)
import Data.Ord (comparing)

-- | A sampler draws a value from a distribution using a random generator.
type Sampler a = StdGen -> (a, StdGen)

-- | Sample a value from a sampler.
sample :: Sampler a -> StdGen -> (a, StdGen)
sample = id

-- | Produce two independent draws from the same sampler.
doubleSamples :: Sampler a -> StdGen -> ((a, a), StdGen)
doubleSamples s g0 =
  let (x, g1) = s g0
      (y, g2) = s g1
   in ((x, y), g2)

-- | Uniform distribution on a closed interval.
uniform :: Double -> Double -> Sampler Double
uniform lo hi g = randomR (lo, hi) g

-- | Compute thresholds by taking the first (sample) draw of each distribution.
thresholds :: [Sampler Double] -> StdGen -> ([Double], StdGen)
thresholds [] g = ([], g)
thresholds (s : ss) g =
  let (t, g1) = s g
      (ts, g2) = thresholds ss g1
   in (t : ts, g2)

-- | Perform the online selection: first actual value that meets its threshold.
select :: [Double] -> [Double] -> Maybe Double
select [] [] = Nothing
select (thr : ths) (val : vs) = if val >= thr then Just val else select ths vs
select _ _ = Nothing

-- | Single‑sample prophet inequality algorithm.
--   For each distribution we draw a sample (threshold) and later an actual value.
singleSampleProphet :: [Sampler Double] -> StdGen -> (Maybe Double, StdGen)
singleSampleProphet samplers gen0 =
  let (ths, gen1) = thresholds samplers gen0
      (actuals, gen2) = mapAccumL (\g s -> let (v, g') = s g in (g', v)) gen1 samplers
   in (select ths actuals, gen2)

-- | Helper to run the algorithm on n i.i.d. uniform[0,1] distributions.
runUniformProphet :: Int -> StdGen -> (Maybe Double, StdGen)
runUniformProphet n gen = singleSampleProphet (replicate n (uniform 0 1)) gen
