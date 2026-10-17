module Types where

type Vertex = Int

-- | Symmetric adjacency matrix with zero diagonal.
type Graph = [[Double]]

newtype Tour = Tour { unTour :: [Vertex] }
  deriving (Eq, Show)

data SAParams = SAParams
  { initialTemp :: Double   -- ^ Starting temperature
  , finalTemp   :: Double   -- ^ Temperature at which the algorithm stops
  , alpha       :: Double   -- ^ Multiplicative cooling factor (0 < alpha < 1)
  , iterPerTemp :: Int      -- ^ Number of neighbour trials per temperature level
  } deriving (Eq, Show)
