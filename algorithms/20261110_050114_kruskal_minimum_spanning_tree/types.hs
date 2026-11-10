module Types where

-- | An undirected weighted edge between two vertices identified by Int.
data Edge = Edge
  { from   :: Int   -- ^ source vertex (0‑based)
  , to     :: Int   -- ^ destination vertex (0‑based)
  , weight :: Int   -- ^ non‑negative weight
  } deriving (Eq, Show)

-- | A graph is simply a list of edges.
type Graph = [Edge]
