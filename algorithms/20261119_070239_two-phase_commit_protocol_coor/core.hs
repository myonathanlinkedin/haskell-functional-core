module Core (
    ParticipantId,
    Vote(..),
    Decision(..),
    Message(..),
    CoordinatorState(..),
    ParticipantState(..),
    initCoordinator,
    initParticipant,
    coordinatorReceive,
    participantReceive,
    runTwoPhaseCommit
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Identifier for a participant.
type ParticipantId = Int

-- | Vote that a participant can send.
data Vote = Commit | Abort
    deriving (Eq, Show)

-- | Final decision made by the coordinator.
data Decision = CommitDecision | AbortDecision
    deriving (Eq, Show)

-- | Messages exchanged between coordinator and participants.
data Message
    = Prepare                     -- ^ Coordinator asks participants to vote.
    | VoteMsg Vote                -- ^ Participant replies with its vote.
    | CommitMsg                   -- ^ Coordinator tells participants to commit.
    | AbortMsg                    -- ^ Coordinator tells participants to abort.
    deriving (Eq, Show)

-- | Internal state of the coordinator.
data CoordinatorState
    = CInit [ParticipantId]                     -- ^ Not started, knows participants.
    | CWaitingVotes (Map ParticipantId (Maybe Vote)) -- ^ Waiting for votes.
    | CDecisionMade Decision                    -- ^ Decision computed, about to notify.
    | CCompleted Decision                       -- ^ All participants have been notified.
    deriving (Eq, Show)

-- | Internal state of a participant.
data ParticipantState
    = PReady                                     -- ^ Awaiting Prepare.
    | PVoted Vote                                -- ^ Vote already sent, awaiting decision.
    | PCommitted                                 -- ^ Received Commit.
    | PAborted                                   -- ^ Received Abort.
    deriving (Eq, Show)

-- | Create an initial coordinator state given the list of participants.
initCoordinator :: [ParticipantId] -> CoordinatorState
initCoordinator pids = CInit pids

-- | Create an initial participant state.
initParticipant :: ParticipantState
initParticipant = PReady

-- | Process a message received by the coordinator from a participant.
--   Returns the updated coordinator state.
coordinatorReceive :: CoordinatorState -> ParticipantId -> Message -> CoordinatorState
coordinatorReceive (CInit pids) pid Prepare =
    -- Transition to waiting for votes, initialise map with Nothing.
    CWaitingVotes $ Map.fromList [(p, Nothing) | p <- pids]
coordinatorReceive (CWaitingVotes votes) pid (VoteMsg v) =
    case Map.lookup pid votes of
        Just Nothing ->
            let votes' = Map.insert pid (Just v) votes
            in if allVotesReceived votes'
               then decide votes'
               else CWaitingVotes votes'
        _ -> CWaitingVotes votes   -- duplicate or unknown vote ignored
  where
    allVotesReceived :: Map ParticipantId (Maybe Vote) -> Bool
    allVotesReceived = all (/= Nothing) . Map.elems

    decide :: Map ParticipantId (Maybe Vote) -> CoordinatorState
    decide vm =
        if any (== Just Abort) (Map.elems vm)
        then CDecisionMade AbortDecision
        else CDecisionMade CommitDecision
coordinatorReceive (CDecisionMade d) _ CommitMsg = CCompleted d
coordinatorReceive (CDecisionMade d) _ AbortMsg  = CCompleted d
coordinatorReceive st _ _ = st   -- any other message leaves state unchanged

-- | Process a message received by a participant.
--   Returns the new participant state and optionally a reply message.
participantReceive :: ParticipantState -> Vote -> Message -> (ParticipantState, Maybe Message)
participantReceive PReady _ Prepare = (PVoted v, Just (VoteMsg v))
  where
    -- The participant's predetermined vote is supplied as an argument.
    v = v
participantReceive (PVoted _) _ (CommitMsg) = (PCommitted, Nothing)
participantReceive (PVoted _) _ (AbortMsg)  = (PAborted, Nothing)
participantReceive st _ _ = (st, Nothing)

-- | Run a full two‑phase commit simulation.
--   The coordinator knows the list of participants.
--   The map argument supplies each participant's predetermined vote.
--   Returns the final coordinator state and a map of final participant states.
runTwoPhaseCommit :: [ParticipantId] -> Map ParticipantId Vote -> (CoordinatorState, Map ParticipantId ParticipantState)
runTwoPhaseCommit pids voteMap = goCoord initC initPMap
  where
    initC = initCoordinator pids
    initPMap = Map.fromList [(pid, initParticipant) | pid <- pids]

    -- Coordinator initiates the protocol.
    goCoord :: CoordinatorState -> Map ParticipantId ParticipantState -> (CoordinatorState, Map ParticipantId ParticipantState)
    goCoord cstate pstateMap =
        case cstate of
            CInit _ ->
                let cstate' = coordinatorReceive cstate (-1) Prepare
                in goCoord cstate' pstateMap
            CWaitingVotes _ ->
                -- Deliver Prepare to each participant and collect votes.
                let (cstate', pstateMap') = foldl handleParticipant (cstate, pstateMap) pids
                in goCoord cstate' pstateMap'
            CDecisionMade decision ->
                let msg = case decision of
                            CommitDecision -> CommitMsg
                            AbortDecision  -> AbortMsg
                    cstate' = coordinatorReceive cstate (-1) msg
                in goCoord cstate' pstateMap
            CCompleted _ -> (cstate, pstateMap)

    handleParticipant :: (CoordinatorState, Map ParticipantId ParticipantState)
                      -> ParticipantId
                      -> (CoordinatorState, Map ParticipantId ParticipantState)
    handleParticipant (cst, pstMap) pid =
        case Map.lookup pid pstMap of
            Just pstate ->
                let predeterminedVote = voteMap Map.! pid
                    (pstate', mReply) = participantReceive pstate predeterminedVote Prepare
                    pstMap' = Map.insert pid pstate' pstMap
                    cst' = case mReply of
                             Just (VoteMsg v) -> coordinatorReceive cst pid (VoteMsg v)
                             _                -> cst
                in (cst', pstMap')
            Nothing -> (cst, pstMap)   -- should not happen
