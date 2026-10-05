module Types where

type Row = Int

data SortResult = SortResult
  { sortedRows :: [Row]
  , timeGen    :: Double   -- seconds spent generating data
  , timeSort   :: Double   -- seconds spent sorting data
  } deriving (Show, Eq)
