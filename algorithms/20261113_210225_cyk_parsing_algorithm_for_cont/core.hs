module CYK (NonTerminal, Terminal, Production(..), Grammar, cyk) where

import Prelude
import Data.List (nub)

-- | Non-terminal symbols are represented as strings.
type NonTerminal = String

-- | Terminal symbols are characters.
type Terminal = Char

-- | Production rules in Chomsky Normal Form.
--   * TermProd A a   represents A → a
--   * BinProd  A B C represents A → B C
data Production
    = TermProd NonTerminal Terminal
    | BinProd  NonTerminal NonTerminal NonTerminal
    deriving (Eq, Show)

type Grammar = [Production]

-- | The CYK parsing algorithm.
--   Returns True iff the start symbol can derive the given input string.
cyk :: Grammar      -- ^ Grammar in CNF
    -> NonTerminal  -- ^ Start symbol
    -> [Terminal]   -- ^ Input string (list of terminals)
    -> Bool
cyk grammar start input = case n of
    0 -> False
    _ -> start `elem` (table !! 0 !! (n - 1))
  where
    n :: Int
    n = length input

    -- Upper‑triangular table: table !! i !! j corresponds to substring
    -- starting at i with length j+1.
    table :: [[ [NonTerminal] ]]
    table = [ [ cell i j | j <- [0 .. n - i - 1] ] | i <- [0 .. n - 1] ]

    -- Compute the set of non‑terminals that derive the substring
    -- starting at i with length j+1.
    cell :: Int -> Int -> [NonTerminal]
    cell i 0 = -- substrings of length 1 (terminals)
        [ lhs | TermProd lhs term <- grammar, term == input !! i ]
    cell i j = nub $ concat
        [ deriveFromSplit i k (j - k - 1)
        | k <- [0 .. j - 1] ]

    -- Combine results of a split at position k.
    deriveFromSplit :: Int -> Int -> Int -> [NonTerminal]
    deriveFromSplit i k l =
        [ lhs
        | BinProd lhs b c <- grammar
        , b `elem` (table !! i !! k)
        , c `elem` (table !! (i + k + 1) !! l)
        ]
