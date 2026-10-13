module Engine (secretSanta) where

import Types
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (sortOn)

-- | Compute a fair Secret Santa assignment respecting exclusions.
--   Returns 'Nothing' if no valid assignment exists.
secretSanta :: [Participant] -> [Exclusion] -> Maybe Assignment
secretSanta participants exclusions = 
    let participantsSet = Set.fromList participants
        -- Build the allowed receivers map for each giver.
        allowedMap = Map.fromList
            [ (g, allowedReceivers g participantsSet exclusions)
            | g <- participants
            ]
        -- All receivers are initially all participants.
        allReceivers = participantsSet
    in solve allowedMap allReceivers []

-- | Compute the set of allowed receivers for a given giver.
allowedReceivers :: Participant -> Set Participant -> [Exclusion] -> Set Participant
allowedReceivers giver allParts excl = 
    let selfRemoved = Set.delete giver allParts
        excluded = Set.fromList [ r | (g, r) <- excl, g == giver ]
    in Set.difference selfRemoved excluded

-- | Recursive backtracking solver.
solve :: Map Participant (Set Participant)   -- ^ Remaining givers with their allowed receivers.
      -> Set Participant                     -- ^ Receivers still unassigned.
      -> Assignment                          -- ^ Accumulated assignments (in reverse order).
      -> Maybe Assignment
solve allowedMap remainingReceivers assigned
    | Map.null allowedMap = Just (reverse assigned)  -- all givers assigned
    | otherwise =
        let -- For each giver, compute the viable receivers intersecting the remaining set.
            candidates :: [(Participant, [Participant])]
            candidates = 
                [ (g, Set.toList $ Set.intersection opts remainingReceivers)
                | (g, opts) <- Map.toList allowedMap
                ]
            -- Choose the giver with the fewest viable options (MRV heuristic).
            sortedCandidates = sortOn (length . snd) candidates
        in case sortedCandidates of
            [] -> Nothing  -- should not happen
            (giver, viable):_
                | null viable -> Nothing  -- dead end
                | otherwise -> tryOptions giver viable
  where
    tryOptions :: Participant -> [Participant] -> Maybe Assignment
    tryOptions _ [] = Nothing
    tryOptions giver (r:rs) =
        let newAllowedMap = Map.delete giver allowedMap
            newRemaining = Set.delete r remainingReceivers
            newAssigned = (giver, r) : assigned
        in case solve newAllowedMap newRemaining newAssigned of
            Just sol -> Just sol
            Nothing  -> tryOptions giver rs
