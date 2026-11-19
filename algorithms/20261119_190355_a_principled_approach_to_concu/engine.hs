module Engine where

import Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isNothing)

-- | Create an initial local state for a participant.
initialLocalState :: LocalState
initialLocalState = LocalState M.empty M.empty []

-- | Create an initial choreography state.
initialChoreoState :: Set Participant -> Choreography -> ChoreoState
initialChoreoState parts choreo = ChoreoState
  { participants = parts
  , localStates  = M.fromList [(p, initialLocalState) | p <- S.toList parts]
  , stepIndex    = 0
  , choreo       = choreo
  , completed    = False
  , errors       = []
  }

-- | Execute a single step, updating the global state.
--   Returns (newState, errorOrNothing).
executeStep :: ChoreoState -> Step -> (ChoreoState, Maybe String)
executeStep st step =
  case step of
    Send p q m ->
      if p `S.member` (participants st) && q `S.member` (participants st)
        then
          let lsP = fromMaybe initialLocalState (M.lookup p (localStates st))
              lsQ = fromMaybe initialLocalState (M.lookup q (localStates st))
              newLsP = lsP
                { pendingSends = M.insertWith (++) q [m] (pendingSends lsP)
                , log = log lsP ++ [step]
                }
              newLsQ = lsQ
                { pendingRecvs = M.insertWith (++) p [m] (pendingRecvs lsQ)
                , log = log lsQ ++ [step]
                }
              newLS = M.insert p newLsP (M.insert q newLsQ (localStates st))
          in (st { localStates = newLS, stepIndex = stepIndex st + 1 }, Nothing)
        else (st { errors = errors st ++ ["Send: unknown participant"] },
              Just "Send: unknown participant")
    Recv p q m ->
      if p `S.member` (participants st) && q `S.member` (participants st)
        then
          let lsP = fromMaybe initialLocalState (M.lookup p (localStates st))
              lsQ = fromMaybe initialLocalState (M.lookup q (localStates st))
              expected = fromMaybe [] (M.lookup q (pendingRecvs lsP))
              in
                if m `elem` expected
                  then
                    let newLsP = lsP
                          { pendingRecvs = M.insert q (filter (/= m) expected) (pendingRecvs lsP)
                          , log = log lsP ++ [step]
                          }
                        newLsQ = lsQ
                          { pendingSends = M.insertWith (filter (/= m)) p [] (pendingSends lsQ)
                          , log = log lsQ ++ [step]
                          }
                        newLS = M.insert p newLsP (M.insert q newLsQ (localStates st))
                    in (st { localStates = newLS, stepIndex = stepIndex st + 1 }, Nothing)
                  else (st { errors = errors st ++ ["Recv: unexpected message from " ++ show q] },
                        Just ("Recv: unexpected message from " ++ show q))
        else (st { errors = errors st ++ ["Recv: unknown participant"] },
              Just "Recv: unknown participant")

-- | Execute the entire choreography from the current state.
--   Returns the final state and a list of messages (errors or info).
executeChoreo :: ChoreoState -> ExecResult
executeChoreo st
  | stepIndex st >= length (choreo st) =
      ExecResult st True []
  | otherwise =
      let step = (choreo st) !! (stepIndex st)
          (st', err) = executeStep st step
          result = executeChoreo st'
      in
        if isNothing err
          then result
          else ExecResult st' False (fromMaybe "" err : messages result)

-- | Run a choreography from scratch.
runChoreo :: Set Participant -> Choreography -> ExecResult
runChoreo parts choreo = executeChoreo (initialChoreoState parts choreo)

-- | Verify that all pending sends and receives are resolved (no dangling messages).
verifyCompletion :: ChoreoState -> Bool
verifyCompletion st =
  all (\ls -> M.null (pendingSends ls) && M.null (pendingRecvs ls))
       (M.elems (localStates st))

-- | Check that the choreography is well-formed: every send has a matching recv and vice versa.
checkWellFormed :: Choreography -> Bool
checkWellFormed choreo =
  let sends = [ (p, q, m) | Send p q m <- choreo ]
      recvs = [ (p, q, m) | Recv p q m <- choreo ]
      -- For each send (p sends to q), there must be a recv (q receives from p)
      sendSet = S.fromList [ (p, q, m) | (p, q, m) <- sends ]
      recvSet = S.fromList [ (q, p, m) | (p, q, m) <- recvs ]
  in S.null (S.difference sendSet recvSet) && S.null (S.difference recvSet sendSet)

-- | Nondeterministic execution: try all branches of a Choice and return all possible results.
executeChoice :: Set Participant -> Choice -> [ExecResult]
executeChoice parts (Choice brs _) = [ runChoreo parts b | b <- brs ]

-- | Asynchronous execution: interleave two choreographies.
--   Returns all possible interleavings (up to a limit to avoid explosion).
interleave :: Int -> Choreography -> Choreography -> [Choreography]
interleave 0 _ _ = []
interleave n a b
  | null a = [b]
  | null b = [a]
  | otherwise =
      let (x:xs) = a
          (y:ys) = b
      in [ x : r | r <- interleave (n-1) xs b ]
       ++ [ y : r | r <- interleave (n-1) a ys ]

-- | Execute all interleavings of two choreographies.
executeAsync :: Set Participant -> Choreography -> Choreography -> [ExecResult]
executeAsync parts a b = [ runChoreo parts il | il <- interleave 20 a b ]
