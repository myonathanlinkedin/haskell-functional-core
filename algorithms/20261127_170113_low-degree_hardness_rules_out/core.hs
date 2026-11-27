module Core where

-- Pure matrix and vector types using Rational for exact arithmetic
type Matrix = [[Rational]]
type Vector = [Rational]

-- | Dot product of two equal‑length vectors.
dot :: Vector -> Vector -> Rational
dot xs ys = sum $ zipWith (*) xs ys

-- | Multiply a square matrix by a vector.
--   The matrix must have the same number of rows as the length of the vector.
matVecMul :: Matrix -> Vector -> Vector
matVecMul m v = map (`dot` v) m

-- | Construct an n×n identity matrix.
identity :: Int -> Matrix
identity n = [ [ if i == j then 1 else 0 | j <- [1..n] ] | i <- [1..n] ]

-- | Construct a diagonal matrix from a list of diagonal entries.
diagonal :: [Rational] -> Matrix
diagonal ds = [ [ if i == j then ds !! (i-1) else 0 | j <- [1..len] ] | i <- [1..len] ]
  where len = length ds

-- | Determine the eigenvalue λ such that A·v = λ·v, if it exists.
--   Returns Nothing for the zero vector or when no single λ satisfies the equation.
eigenvalueIfEigenvector :: Matrix -> Vector -> Maybe Rational
eigenvalueIfEigenvector a v
  | null v || all (==0) v = Nothing                     -- zero vector cannot be an eigenvector
  | otherwise =
      case firstNonZeroIndex v of
        Nothing -> Nothing                               -- should not happen (handled above)
        Just i  ->
          let vi = v !! i
              wi = w !! i
              lambda = wi / vi
          in if all (checkEntry lambda) (zip v w) then Just lambda else Nothing
  where
    w = matVecMul a v
    -- Find the first index where the vector entry is non‑zero.
    firstNonZeroIndex :: Vector -> Maybe Int
    firstNonZeroIndex xs = go 0 xs
      where
        go _ [] = Nothing
        go n (y:ys)
          | y /= 0    = Just n
          | otherwise = go (n+1) ys
    -- Verify vi * λ == wi for each component, taking care of zero entries.
    checkEntry :: Rational -> (Rational, Rational) -> Bool
    checkEntry λ (vi, wi) = vi * λ == wi

-- | Boolean predicate: does the vector qualify as an eigenvector of the matrix?
isEigenvector :: Matrix -> Vector -> Bool
isEigenvector a v = case eigenvalueIfEigenvector a v of
                      Just _  -> True
                      Nothing -> False
