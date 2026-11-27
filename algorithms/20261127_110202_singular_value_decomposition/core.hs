module Core (
    Matrix,
    Vector,
    svdLowRank,
    reconstruct,
    approxEqualMatrix
) where

type Matrix = [[Double]]
type Vector = [Double]

-- Basic linear algebra utilities ----------------------------------------------

transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

matMul :: Matrix -> Matrix -> Matrix
matMul a b = let bt = transpose b
             in [[ sum $ zipWith (*) ar bc | bc <- bt ] | ar <- a ]

matVecMul :: Matrix -> Vector -> Vector
matVecMul m v = [ sum $ zipWith (*) row v | row <- m ]

dot :: Vector -> Vector -> Double
dot = (sum .) . zipWith (*)

vecAdd :: Vector -> Vector -> Vector
vecAdd = zipWith (+)

vecSub :: Vector -> Vector -> Vector
vecSub = zipWith (-)

vecScale :: Double -> Vector -> Vector
vecScale s = map (s*)

vecNorm :: Vector -> Double
vecNorm v = sqrt $ dot v v

normalize :: Vector -> Vector
normalize v = let n = vecNorm v in if n == 0 then v else vecScale (1 / n) v

outerProduct :: Vector -> Vector -> Matrix
outerProduct u v = [ [ ui * vj | vj <- v ] | ui <- u ]

matAdd :: Matrix -> Matrix -> Matrix
matAdd = zipWith (zipWith (+))

matScale :: Double -> Matrix -> Matrix
matScale s = map (map (s*))

-- Power iteration for dominant eigenpair of a symmetric matrix --------------

powerIteration :: Matrix -> Int -> Double -> (Vector, Double)
powerIteration b maxIter tol = go initVec 0
  where
    n = length b
    initVec = normalize (replicate n 1.0)
    go v iter
      | iter >= maxIter = (v, lambda v)
      | vecNorm (vecSub v' v) < tol = (v', lambda v')
      | otherwise = go v' (iter + 1)
      where
        v' = normalize (matVecMul b v)
    lambda v = dot v (matVecMul b v)

-- Deflation of a rank‑1 component ----------------------------------------------

deflate :: Matrix -> Double -> Vector -> Vector -> Matrix
deflate a sigma u v = matSub a (matScale sigma (outerProduct u v))

matSub :: Matrix -> Matrix -> Matrix
matSub = zipWith (zipWith (-))

-- Low‑rank SVD using successive power iterations and deflation --------------

svdLowRank :: Int -> Matrix -> (Matrix, [Double], [Vector], [Vector])
svdLowRank k a = go k a [] [] []
  where
    go 0 _ ss us vs = (reconstruct (reverse ss) (reverse us) (reverse vs), reverse ss, reverse us, reverse vs)
    go i aCurr ss us vs =
        let bt = matMul (transpose aCurr) aCurr
            (v, lambda) = powerIteration bt 1000 1e-10
            sigma = sqrt lambda
        in if sigma < 1e-12
           then (reconstruct (reverse ss) (reverse us) (reverse vs), reverse ss, reverse us, reverse vs)
           else
               let u = normalize (matVecMul aCurr v)
                   aNext = deflate aCurr sigma u v
               in go (i - 1) aNext (sigma:ss) (u:us) (v:vs)

-- Reconstruct matrix from singular components ----------------------------------

reconstruct :: [Double] -> [Vector] -> [Vector] -> Matrix
reconstruct ss us vs = foldl matAdd (zeroMatrix rows cols) components
  where
    rows = length (head us)
    cols = length (head vs)
    zeroMatrix r c = replicate r (replicate c 0.0)
    components = zipWith3 (\s u v -> matScale s (outerProduct u v)) ss us vs

-- Approximate equality for testing -------------------------------------------

approxEqualMatrix :: Double -> Matrix -> Matrix -> Bool
approxEqualMatrix eps a b = and $ zipWith rowEq a b
  where
    rowEq ra rb = and $ zipWith (\x y -> abs (x - y) <= eps) ra rb
