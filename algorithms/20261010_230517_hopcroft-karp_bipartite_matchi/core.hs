module Core (maxMatchingSize, maxMatching) where

import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type Vertex = Int
type AdjList = Array Vertex [Vertex]

-- | Compute only the size of a maximum matching.
maxMatchingSize :: Int -> Int -> [(Vertex, Vertex)] -> Int
maxMatchingSize nL nR edges = let (size,_,_) = maxMatching nL nR edges in size

-- | Compute a maximum matching. Returns (size, pairU, pairV)
--   pairU!u = v matched to u (0 = unmatched)
--   pairV!v = u matched to v (0 = unmatched)
maxMatching :: Int -> Int -> [(Vertex, Vertex)] -> (Int, Array Vertex Vertex, Array Vertex Vertex)
maxMatching nL nR edges = runST $ do
    let adj = buildAdj nL edges
    pairU <- newArray (1, nL) 0 :: ST s (STArray s Vertex Vertex)
    pairV <- newArray (1, nR) 0 :: ST s (STArray s Vertex Vertex)
    dist  <- newArray (0, nL) (maxBound :: Int) :: ST s (STArray s Vertex Int)

    let bfs :: ST s Bool
        bfs = do
            let nil = 0
            writeArray dist nil (maxBound :: Int)
            q0 <- foldM (\q u -> do
                    pu <- readArray pairU u
                    if pu == nil
                        then writeArray dist u 0 >> return (q Seq.|> u)
                        else writeArray dist u (maxBound :: Int) >> return q
                ) Seq.empty [1..nL]
            bfsLoop q0
          where
            bfsLoop q = case Seq.viewl q of
                Seq.EmptyL -> do
                    d0 <- readArray dist 0
                    return (d0 /= maxBound)
                u Seq.:< rest -> do
                    du <- readArray dist u
                    d0 <- readArray dist 0
                    when (du < d0) $ do
                        forM_ (adj ! u) $ \v -> do
                            pu <- readArray pairV v
                            if pu == 0
                                then do
                                    d0' <- readArray dist 0
                                    when (d0' == maxBound) $ writeArray dist 0 (du + 1)
                                else do
                                    dp <- readArray dist pu
                                    when (dp == maxBound) $ do
                                        writeArray dist pu (du + 1)
                                        bfsLoop (rest Seq.|> pu) >> return ()
                    bfsLoop rest

    let recDFS :: Vertex -> ST s Bool
        recDFS u = if u == 0
            then return True
            else do
                let nil = 0
                du <- readArray dist u
                let tryV [] = do
                        writeArray dist u (maxBound :: Int)
                        return False
                    tryV (v:vs) = do
                        pu <- readArray pairV v
                        dp <- readArray dist pu
                        let cond = (pu == nil && du + 1 == (maxBound :: Int) && False) -- placeholder
                        if (pu == nil && du + 1 == (maxBound :: Int)) then tryV vs else return ()
                        if (pu == nil && du + 1 == (maxBound :: Int)) then tryV vs else return ()
                -- proper condition below
                let go [] = do
                        writeArray dist u (maxBound :: Int)
                        return False
                    go (v:vs) = do
                        pu <- readArray pairV v
                        dp <- readArray dist pu
                        if (pu == 0 && du + 1 == (maxBound :: Int)) || (pu /= 0 && dp == du + 1)
                            then do
                                ok <- recDFS pu
                                if ok
                                    then do
                                        writeArray pairU u v
                                        writeArray pairV v u
                                        return True
                                    else go vs
                            else go vs
                go (adj ! u)

    let dfs :: Vertex -> ST s Bool
        dfs u = if u == 0 then return True else do
            du <- readArray dist u
            let try [] = do
                    writeArray dist u (maxBound :: Int)
                    return False
                try (v:vs) = do
                    pu <- readArray pairV v
                    dp <- readArray dist pu
                    if (pu == 0 && du + 1 == (maxBound :: Int)) || (pu /= 0 && dp == du + 1)
                        then do
                            ok <- dfs pu
                            if ok
                                then do
                                    writeArray pairU u v
                                    writeArray pairV v u
                                    return True
                                else try vs
                        else try vs
            try (adj ! u)

    let augment :: Int -> ST s Int
        augment matching = do
            bfsResult <- bfs
            if not bfsResult
                then return matching
                else do
                    forM_ [1..nL] $ \u -> do
                        pu <- readArray pairU u
                        when (pu == 0) $ do
                            _ <- dfs u
                            return ()
                    let countMatches = foldM (\cnt u -> do pu <- readArray pairU u
                                                          return $ if pu /= 0 then cnt + 1 else cnt) 0 [1..nL]
                    newMatch <- countMatches
                    augment newMatch

    finalMatch <- augment 0
    finalPairU <- freeze pairU
    finalPairV <- freeze pairV
    return (finalMatch, finalPairU, finalPairV)

-- Build adjacency array from edge list
buildAdj :: Int -> [(Vertex, Vertex)] -> AdjList
buildAdj nL edges = accumArray (flip (:)) [] (1, nL) edges
