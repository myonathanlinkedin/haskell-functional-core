module Core (SAM, buildSAM, contains, occurrences) where

import Data.List (foldl', sortOn)
import Data.Maybe (fromMaybe)

-- State of the suffix automaton
data State = State
  { stLen   :: Int               -- length of longest string in this equivalence class
  , stLink  :: Int               -- suffix link
  , stTrans :: [(Char, Int)]     -- transitions: character -> state index
  , stOcc   :: Int               -- number of end positions (for occurrence counting)
  } deriving (Show)

type SAM = [State]   -- index in the list is the state identifier

-- Helper: replace element at index i with f applied to it
updateState :: Int -> (State -> State) -> SAM -> SAM
updateState i f sam = zipWith (\idx st -> if idx == i then f st else st) [0..] sam

-- Helper: add or replace a transition in a state's transition list
addTrans :: Char -> Int -> [(Char, Int)] -> [(Char, Int)]
addTrans c v [] = [(c,v)]
addTrans c v ((c',v'):xs)
  | c == c'   = (c,v):xs
  | otherwise = (c',v') : addTrans c v xs

-- Retrieve transition if exists
lookupTrans :: Char -> [(Char, Int)] -> Maybe Int
lookupTrans c = lookup c

-- Build the suffix automaton for a given string
buildSAM :: String -> SAM
buildSAM s = finalSAM
  where
    initState = State { stLen = 0, stLink = -1, stTrans = [], stOcc = 0 }
    (finalSAM, _) = foldl' extend ( [initState] , 0 ) s

    extend :: (SAM, Int) -> Char -> (SAM, Int)
    extend (sam, lastIdx) c = (sam'', curIdx)
      where
        curIdx = length sam
        curState = State { stLen = stLen (sam !! lastIdx) + 1
                         , stLink = 0
                         , stTrans = []
                         , stOcc = 1 }
        sam' = sam ++ [curState]

        -- Step 1: add transitions for states that lack c
        (sam1, p) = addTransLoop sam' lastIdx
        addTransLoop st pIdx
          | pIdx == -1 = (st, -1)
          | otherwise =
              case lookupTrans c (stTrans (st !! pIdx)) of
                Nothing ->
                  let st' = updateState pIdx (\stt -> stt { stTrans = addTrans c curIdx (stTrans stt) }) st
                  in addTransLoop st' (stLink (st' !! pIdx))
                Just _ -> (st, pIdx)

        -- Step 2: set suffix link for curIdx
        (sam2, linkIdx) = case p of
          -1 -> (sam1, 0)
          _  ->
            let qIdx = fromMaybe (error "Impossible") (lookupTrans c (stTrans (sam1 !! p)))
                q = sam1 !! qIdx
            in if stLen (sam1 !! p) + 1 == stLen q
               then (sam1, qIdx)
               else
                 let cloneIdx = length sam1
                     cloneState = State { stLen = stLen (sam1 !! p) + 1
                                        , stLink = stLink q
                                        , stTrans = stTrans q
                                        , stOcc = 0 }
                     samClone = sam1 ++ [cloneState]
                     samFixQ = updateState qIdx (\stt -> stt { stLink = cloneIdx }) samClone
                     samFixCur = updateState curIdx (\stt -> stt { stLink = cloneIdx }) samFixQ
                     samFinal = fixTransLoop samFixCur p
                     fixTransLoop st pIdx
                       | pIdx == -1 = st
                       | otherwise =
                           case lookupTrans c (stTrans (st !! pIdx)) of
                             Just tIdx | tIdx == qIdx ->
                               let st' = updateState pIdx (\stt -> stt { stTrans = addTrans c cloneIdx (stTrans stt) }) st
                               in fixTransLoop st' (stLink (st' !! pIdx))
                             _ -> st
                 in (samFinal, cloneIdx)

        sam'' = updateState curIdx (\stt -> stt { stLink = linkIdx }) sam2

-- Check if a pattern exists as a substring
contains :: SAM -> String -> Bool
contains sam = go 0
  where
    go _ [] = True
    go cur (c:cs) =
      case lookupTrans c (stTrans (sam !! cur)) of
        Just nxt -> go nxt cs
        Nothing  -> False

-- Compute occurrence counts for all states (must be called after building SAM)
propagateOcc :: SAM -> SAM
propagateOcc sam = foldl' addOcc sam order
  where
    -- states sorted by decreasing length
    order = map fst $ reverse $ sortOn snd $ zip [0..] (map stLen sam)
    addOcc st idx =
      let linkIdx = stLink (st !! idx)
      in if linkIdx /= -1
         then updateState linkIdx (\s -> s { stOcc = stOcc s + stOcc (st !! idx) }) st
         else st

-- Number of occurrences of a pattern (0 if not present)
occurrences :: SAM -> String -> Int
occurrences sam pat = case walk 0 pat of
    Just idx -> stOcc (samProp !! idx)
    Nothing  -> 0
  where
    samProp = propagateOcc sam
    walk _ [] = Just 0
    walk cur (c:cs) =
      case lookupTrans c (stTrans (samProp !! cur)) of
        Just nxt -> walk nxt cs
        Nothing  -> Nothing
