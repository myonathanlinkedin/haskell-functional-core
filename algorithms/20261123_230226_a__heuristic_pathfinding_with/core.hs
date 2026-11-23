module Core (
    Pos,
    aStar,
    manhattan,
    Grid,
    mkGrid
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)

-- | 2‑D position on the grid.
type Pos = (Int, Int)

-- | Grid represented as a function from position to base movement cost.
--   Nothing denotes an impassable cell.
type Grid = Pos -> Maybe Int

-- | Construct a grid from a list of rows.  A value of -1 indicates impassable.
mkGrid :: [[Int]] -> Grid
mkGrid rows = \pos@(x,y) ->
    if y < 0 || y >= height || x < 0 || x >= width then Nothing
    else let v = (rows !! y) !! x
         in if v < 0 then Nothing else Just v
  where
    height = length rows
    width  = if null rows then 0 else length (head rows)

-- | Manhattan distance heuristic.
manhattan :: Pos -> Pos -> Int
manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Simple priority queue based on an ordered list.
type PQ = [(Int, Pos)]   -- ^ (priority, element)

pqInsert :: (Int, Pos) -> PQ -> PQ
pqInsert e [] = [e]
pqInsert e@(p,_) q@((p',_):_)
    | p <= p'   = e : q
    | otherwise = (p',snd (head q)) : pqInsert e (tail q)

pqPop :: PQ -> Maybe ((Int, Pos), PQ)
pqPop [] = Nothing
pqPop (x:xs) = Just (x, xs)

-- | Reconstruct the path from the cameFrom map.
reconstruct :: Map.Map Pos Pos -> Pos -> [Pos]
reconstruct cameFrom current =
    case Map.lookup current cameFrom of
        Nothing -> [current]
        Just prev -> reconstruct cameFrom prev ++ [current]

-- | Generate the four orthogonal neighbours of a position.
neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- | A* search.
--   * grid      – base cost function (Nothing = blocked)
--   * dynCost   – additional dynamic cost per cell (defaults to 0)
--   * start     – start position
--   * goal      – goal position
--   Returns the optimal path as a list of positions, or Nothing if unreachable.
aStar :: Grid               -- ^ base cost map
      -> (Pos -> Int)       -- ^ dynamic cost function
      -> Pos                -- ^ start
      -> Pos                -- ^ goal
      -> Maybe [Pos]
aStar grid dynCost start goal
    | isNothing (grid start) = Nothing
    | isNothing (grid goal)  = Nothing
    | otherwise = search initialOpen Set.empty gScoreMap cameFromMap
  where
    isNothing = not . isJust

    h pos = manhattan pos goal
    fScore startPos = h startPos

    initialOpen = [(fScore start, start)]
    gScoreMap = Map.singleton start 0
    cameFromMap = Map.empty

    -- Main loop
    search :: PQ -> Set.Set Pos -> Map.Map Pos Int -> Map.Map Pos Pos -> Maybe [Pos]
    search open closed gScore cameFrom =
        case pqPop open of
            Nothing -> Nothing
            Just ((_, current), open')
                | current == goal -> Just (reconstruct cameFrom goal)
                | Set.member current closed -> search open' closed gScore cameFrom
                | otherwise ->
                    let closed' = Set.insert current closed
                        (open'', gScore', cameFrom') = foldl (processNeighbour current) (open', gScore, cameFrom) (neighbors current)
                    in search open'' closed' gScore' cameFrom'

    -- Process a single neighbour.
    processNeighbour :: Pos -> (PQ, Map.Map Pos Int, Map.Map Pos Pos) -> Pos -> (PQ, Map.Map Pos Int, Map.Map Pos Pos)
    processNeighbour current (openAcc, gScoreAcc, cameFromAcc) neigh =
        case grid neigh of
            Nothing -> (openAcc, gScoreAcc, cameFromAcc)   -- blocked
            Just baseCost ->
                let tentativeG = fromMaybe maxBound (Map.lookup current gScoreAcc) + baseCost + dynCost neigh
                    neighG = Map.lookup neigh gScoreAcc
                in case neighG of
                    Just existingG | tentativeG >= existingG -> (openAcc, gScoreAcc, cameFromAcc)
                    _ ->
                        let f = tentativeG + h neigh
                            open' = pqInsert (f, neigh) openAcc
                            gScore' = Map.insert neigh tentativeG gScoreAcc
                            cameFrom' = Map.insert neigh current cameFromAcc
                        in (open', gScore', cameFrom')
    -- End of where
