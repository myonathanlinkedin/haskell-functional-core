module Core (hopcroftKarp, Matching) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type Vertex = Int
type AdjList = IM.IntMap [Vertex]

-- Matching from left vertices to right vertices and vice‑versa
data Matching = Matching
  { matchU :: IM.IntMap Vertex   -- ^ left → right
  , matchV :: IM.IntMap Vertex   -- ^ right → left
  } deriving (Eq, Show)

emptyMatching :: Matching
emptyMatching = Matching IM.empty IM.empty

infinity :: Int
infinity = maxBound :: Int

-- Breadth‑first search builds distance layers from all free left vertices.
bfs :: Int -> AdjList -> Matching -> (IM.IntMap Int, Bool)
bfs nU adj Matching{matchU=mU, matchV=mV} = go initialDist initialQueue False
  where
    -- initial distances: 0 for free left vertices, INF otherwise
    initialDist = IM.fromList
      [ (u, if IM.member u mU then infinity else 0) | u <- [1..nU] ]
    initialQueue = Seq.fromList [ u | u <- [1..nU], not (IM.member u mU) ]

    go dist queue foundNil =
      case Seq.viewl queue of
        Seq.EmptyL -> (dist, foundNil)
        u Seq.:< rest ->
          let du = IM.findWithDefault infinity u dist
          in if du < distNil then
               let (dist', queue') = foldl (processEdge du) (dist, rest) (IM.findWithDefault [] u adj)
               in go dist' queue' foundNil
             else go dist queue foundNil
      where
        distNil = IM.findWithDefault infinity 0 dist   -- distance to NIL stored at key 0

    processEdge du (dmap, q) v =
      case IM.lookup v mV of
        Nothing -> -- free right vertex, we can reach NIL
          let dmap' = IM.insert 0 (du + 1) dmap
          in (dmap', q)
        Just u' ->
          if IM.findWithDefault infinity u' dmap == infinity then
            let dmap' = IM.insert u' (du + 1) dmap
                q'    = q Seq.|> u'
            in (dmap', q')
          else (dmap, q)

-- Depth‑first search tries to find an augmenting path from a given left vertex.
dfs :: Vertex -> AdjList -> Matching -> IM.IntMap Int -> (Bool, Matching)
dfs u adj match@Matching{matchU=mU, matchV=mV} dist =
  let try [] = (False, match)
      try (v:vs) =
        case IM.lookup v mV of
          Nothing -> -- free right vertex, augment directly
            let mU' = IM.insert u v mU
                mV' = IM.insert v u mV
            in (True, Matching mU' mV')
          Just u' ->
            let du  = IM.findWithDefault infinity u dist
                du' = IM.findWithDefault infinity u' dist
            in if du' == du + 1 then
                 let (found, match') = dfs u' adj match dist
                 in if found then
                      let mU' = IM.insert u v (matchU match')
                          mV' = IM.insert v u (matchV match')
                      in (True, Matching mU' mV')
                    else try vs
               else try vs
  in try (IM.findWithDefault [] u adj)

-- Main driver: returns size of maximum matching and the matching itself.
hopcroftKarp :: Int -> Int -> AdjList -> (Int, Matching)
hopcroftKarp nU _nV adj = go emptyMatching 0
  where
    go match size =
      let (dist, reachable) = bfs nU adj match
      in if not reachable then (size, match)
         else
           let (match', added) = augmentAll match dist
           in go match' (size + added)

    augmentAll match dist = foldl step (match, 0) freeLeft
      where
        freeLeft = [ u | u <- [1..nU], not (IM.member u (matchU match)) ]
        step (m, acc) u =
          let (found, m') = dfs u adj m dist
          in if found then (m', acc + 1) else (m, acc)
