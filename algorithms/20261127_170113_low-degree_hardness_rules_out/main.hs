module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

-- Simple test representation
data Test = Test { testName :: String, testResult :: Bool }

-- Run a list of tests, printing results and exiting with appropriate status.
runTests :: [Test] -> IO ()
runTests ts = do
    mapM_ report ts
    if all testResult ts
       then putStrLn "All tests passed." >> exitSuccess
       else putStrLn "Some tests failed." >> exitFailure
  where
    report (Test n r) = putStrLn $ n ++ ": " ++ (if r then "PASS" else "FAIL")

-- Helper to compare Maybe Rational values safely.
eqMaybeR :: Maybe Rational -> Maybe Rational -> Bool
eqMaybeR (Just x) (Just y) = x == y
eqMaybeR Nothing Nothing   = True
eqMaybeR _ _               = False

-- Test suite
tests :: [Test]
tests =
  [ Test "Identity matrix any non‑zero vector is eigenvector"
      (let m = identity 3
           v = [1, -2, 3]
       in isEigenvector m v && eqMaybeR (eigenvalueIfEigenvector m v) (Just 1))

  , Test "Diagonal matrix eigenvectors are standard basis vectors"
      (let dVals = [2, 5, -3]
           m = diagonal dVals
           e1 = [1,0,0]
           e2 = [0,1,0]
           e3 = [0,0,1]
        in  isEigenvector m e1 && eqMaybeR (eigenvalueIfEigenvector m e1) (Just 2)
         && isEigenvector m e2 && eqMaybeR (eigenvalueIfEigenvector m e2) (Just 5)
         && isEigenvector m e3 && eqMaybeR (eigenvalueIfEigenvector m e3) (Just (-3))
      )

  , Test "Non‑eigenvector detection"
      (let m = [[0,1],[1,0]]   -- swap matrix
           v = [1,1]           -- not an eigenvector (maps to [1,1] which *is* eigenvector with λ=1? actually swap maps [1,1] to [1,1] so it is eigenvector; choose [1,2])
           v' = [1,2]
        in not (isEigenvector m v') && eigenvalueIfEigenvector m v' == Nothing)

  , Test "Zero vector is not an eigenvector"
      (let m = identity 2
           v = [0,0]
        in not (isEigenvector m v) && eigenvalueIfEigenvector m v == Nothing)

  , Test "Matrix with repeated eigenvalue but distinct eigenvectors"
      (let m = [[3,0,0],[0,3,0],[0,0,3]]  -- 3·I
           v = [4,5,6]
        in isEigenvector m v && eqMaybeR (eigenvalueIfEigenvector m v) (Just 3))
  ]

main :: IO ()
main = runTests tests
