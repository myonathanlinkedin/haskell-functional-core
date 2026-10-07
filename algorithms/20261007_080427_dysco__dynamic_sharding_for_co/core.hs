module Core where

import Data.Char (toUpper)

type Token = String

data Shard = Edge | Cloud deriving (Eq, Show)

type ShardingPlan = [(Shard, [Token])]

-- | Split a list of tokens into an edge shard (up to a capacity) and a cloud shard.
splitTokens :: Int -> [Token] -> ShardingPlan
splitTokens cap toks =
    let (edgeToks, cloudToks) = splitAt cap toks
    in [(Edge, edgeToks), (Cloud, cloudToks)]

-- | Dummy processing of a shard: prefix each token with the shard name and uppercase it.
processShard :: Shard -> [Token] -> [String]
processShard shard = map (\t -> show shard ++ ":" ++ map toUpper t)

-- | Combine processed shard results preserving the original plan order.
combineResults :: ShardingPlan -> [[String]] -> [String]
combineResults _ = concat
