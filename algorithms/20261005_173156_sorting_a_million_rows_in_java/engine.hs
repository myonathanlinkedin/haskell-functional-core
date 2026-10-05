module Engine where

import Types
import System.Random (randomRIO)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (deepseq)
import Data.List (sort)

-- Convert picoseconds to seconds
psToSec :: Integer -> Double
psToSec ps = fromIntegral ps / 1e12

-- Generate n random rows (integers)
generateRows :: Int -> IO [Row]
generateRows n = mapM (\_ -> randomRIO (minBound :: Int, maxBound :: Int)) [1..n]

-- Sort a list of rows while measuring generation and sorting time separately
processRows :: Int -> IO SortResult
processRows n = do
  -- Generation phase
  startGen <- getCPUTime
  rows <- generateRows n
  rows `deepseq` return ()
  endGen <- getCPUTime
  let genTime = psToSec (endGen - startGen)

  -- Sorting phase
  startSort <- getCPUTime
  let sorted = sort rows
  sorted `deepseq` return ()
  endSort <- getCPUTime
  let sortTime = psToSec (endSort - startSort)

  return $ SortResult sorted genTime sortTime
