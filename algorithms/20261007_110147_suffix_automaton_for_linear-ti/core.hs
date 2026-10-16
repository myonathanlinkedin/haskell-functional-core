module Core where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)

data State = State
  { len  :: Int          -- ^ length of longest string in this state
  , link :: Int          -- ^ suffix link
  , next :: Map Char Int -- ^ transitions
  , occ  :: Int          -- ^ number of end positions (occurrences)
  } deriving Show

type SAM = [State]

emptyState :: State
emptyState = State 0 (-1) Map.empty 0

buildSAM :: String -> SAM
buildSAM s = finalize $ foldl addChar initSAM s
  where
    initSAM = [emptyState]  -- state 0 is root

    addChar :: SAM -> Char -> SAM
    addChar sam c =
      let cur = State (len (last sam) + 1) 0 Map.empty 0
          sam' = sam ++ [cur]
          p = last sam
          (sam'', lastState) = extend sam' p (len cur - 1) c
      in sam''

    extend :: SAM -> Int -> Int -> Char -> (SAM, Int)
    extend sam p curLen c
      | p == -1 = (updateLink sam curLen 0, curLen)
      | Map.member c (next (sam !! p)) =
          let q = next (sam !! p) Map.! c
          in if len (sam !! p) + 1 == len (sam !! q)
                then (updateLink sam curLen q, curLen)
                else
                  let clone = State (len (sam !! p) + 1) (link (sam !! q))
                                 (next (sam !! q)) 0
                      cloneIdx = length sam
                      sam' = sam ++ [clone]
                      sam'' = updateLink sam' curLen q
                      sam''' = updateLink sam'' curLen cloneIdx
                      sam'''' = updateTransitions sam''' p c cloneIdx
                  in (sam'''', cloneIdx)
      | otherwise =
          let sam' = updateTransitions sam p c curLen
          in extend sam' (link (sam !! p)) curLen c

    updateLink :: SAM -> Int -> Int -> SAM
    updateLink sam idx newLink =
      let st = sam !! idx
          st' = st { link = newLink }
      in take idx sam ++ [st'] ++ drop (idx + 1) sam

    updateTransitions :: SAM -> Int -> Char -> Int -> SAM
    updateTransitions sam idx ch to =
      let st = sam !! idx
          st' = st { next = Map.insert ch to (next st) }
      in take idx sam ++ [st'] ++ drop (idx + 1) sam

    finalize :: SAM -> SAM
    finalize sam =
      let order = reverse $ sortBy (comparing len) [0 .. length sam - 1]
          sam' = foldl propagate sam order
      in sam'

    propagate :: SAM -> Int -> SAM
    propagate sam v =
      let st = sam !! v
          lnk = link st
      in if lnk /= -1
            then let st' = sam !! lnk
                     st'' = st' { occ = occ st' + occ st }
                     sam' = take lnk sam ++ [st''] ++ drop (lnk + 1) sam
                 in sam'
            else sam

isSubstring :: SAM -> String -> Bool
isSubstring sam [] = True
isSubstring sam (c:cs) =
  case Map.lookup c (next (sam !! 0)) of
    Nothing -> False
    Just nxt -> traverseState sam nxt cs
  where
    traverseState :: SAM -> Int -> String -> Bool
    traverseState _ _ [] = True
    traverseState sam v (x:xs) =
      case Map.lookup x (next (sam !! v)) of
        Nothing -> False
        Just nxt -> traverseState sam nxt xs

substringOccurrences :: SAM -> String -> Int
substringOccurrences sam [] = 0
substringOccurrences sam (c:cs) =
  case Map.lookup c (next (sam !! 0)) of
    Nothing -> 0
    Just nxt -> traverseState sam nxt cs
  where
    traverseState :: SAM -> Int -> String -> Int
    traverseState _ _ [] = occ (sam !! nxt)
    traverseState sam v (x:xs) =
      case Map.lookup x (next (sam !! v)) of
        Nothing -> 0
        Just nxt' -> traverseState sam nxt' xs
