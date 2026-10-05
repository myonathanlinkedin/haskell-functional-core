module Main where

import Data.Complex
import Data.List
import System.Exit (exitFailure)

-- | Check if an integer is a power of two (and > 0)
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0
  where (.&.) = (.&.) .: (&&)  -- placeholder to avoid import; will be replaced

-- Actually we need bitwise AND; use Data.Bits
import Data.Bits ((.&.))

-- | Pad a list with zeros to the next power of two length
padToPowerOfTwo :: [Complex Double] -> [Complex Double]
padToPowerOfTwo xs = xs ++ replicate (nextPow - length xs) 0
  where
    n = length xs
    nextPow = if isPowerOfTwo n then n else 2 ^ (ceiling (logBase 2 (fromIntegral n :: Double)) :: Int)

-- | Cooley–Tukey radix-2 FFT (recursive, in-place style)
fft :: [Complex Double] -> [Complex Double]
fft xs
  | not (isPowerOfTwo n) = fft (padToPowerOfTwo xs)
  | n == 1    = xs
  | otherwise = zipWith (+) evens' oddsTwiddled ++ zipWith (-) evens' oddsTwiddled
  where
    n = length xs
    (evens, odds) = partitionEvensOdds xs
    evens' = fft evens
    odds'  = fft odds
    factor k = cis (-2 * pi * fromIntegral k / fromIntegral n)
    oddsTwiddled = [ odds' !! k * factor k | k <- [0 .. n `div` 2 - 1] ]

-- | Helper to split list into even and odd indexed elements
partitionEvensOdds :: [a] -> ([a], [a])
partitionEvensOdds = foldr (\x (e,o) -> (o, x:e)) ([],[])

-- | Inverse FFT using conjugation method
ifft :: [Complex Double] -> [Complex Double]
ifft xs = map (/ fromIntegral n) $ map conjugate $ fft $ map conjugate xs
  where n = length xs

-- | Simple assertion with tolerance for complex numbers
assertApproxEqual :: String -> Complex Double -> Complex Double -> Double -> IO ()
assertApproxEqual msg expected actual eps =
  if magnitude (expected - actual) <= eps
    then return ()
    else do
      putStrLn $ "Assertion failed: " ++ msg
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Actual  : " ++ show actual
      exitFailure

-- | Unit tests
runTests :: IO ()
runTests = do
  let input = [0,1,0,0] :: [Complex Double]
      expectedFFT = [1 :+ 0, 0 :+ (-1), -1 :+ 0, 0 :+ 1]
      resultFFT = fft input
  assertApproxEqual "FFT of [0,1,0,0]" (head expectedFFT) (head resultFFT) 1e-12
  mapM_ (\(e,a) -> assertApproxEqual "FFT element" e a 1e-12) (zip expectedFFT resultFFT)

  let recovered = ifft resultFFT
  mapM_ (\(e,a) -> assertApproxEqual "iFFT recovery" e a 1e-12) (zip input recovered)

  putStrLn "All tests passed."

main :: IO ()
main = runTests
