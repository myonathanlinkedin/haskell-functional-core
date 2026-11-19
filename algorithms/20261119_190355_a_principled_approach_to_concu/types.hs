module Types where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- | A unique identifier for a participant in the choreography.
data Participant = P1 | P2 | P3 | P4 | P5
  deriving (Eq, Ord, Show, Read)

-- | A message type exchanged between participants.
data Msg = IntMsg Int | BoolMsg Bool | UnitMsg
  deriving (Eq, Show, Read)

-- | A single communication step in a choreography.
--   'Send p q m' means participant p sends message m to participant q.
--   'Recv p q m' means participant p receives message m from participant q.
data Step = Send Participant Participant Msg
          | Recv Participant Participant Msg
  deriving (Eq, Show, Read)

-- | A choreography is a sequence of steps.
type Choreography = [Step]

-- | The state of a single participant's local view.
--   'pendingSends' : messages this participant has sent but not yet confirmed received.
--   'pendingRecvs' : messages this participant expects to receive (with sender).
--   'log'           : the local execution log of completed steps.
data LocalState = LocalState
  { pendingSends :: Map Participant [Msg]
  , pendingRecvs :: Map Participant [Msg]
  , log          :: [Step]
  } deriving (Eq, Show)

-- | The global state of the choreography engine.
data ChoreoState = ChoreoState
  { participants :: Set Participant
  , localStates  :: Map Participant LocalState
  , stepIndex    :: Int
  , choreo       :: Choreography
  , completed    :: Bool
  , errors       :: [String]
  } deriving (Eq, Show)

-- | Result of executing a choreography.
data ExecResult = ExecResult
  { finalState :: ChoreoState
  , success    :: Bool
  , messages   :: [String]
  } deriving (Eq, Show)

-- | Nondeterministic choice: a choreography that may take one of several branches.
data Choice = Choice
  { branches :: [Choreography]
  , guard    :: Maybe (Participant -> Bool)
  } deriving (Eq, Show)

-- | A choreography node in a tree structure supporting asynchrony and nondeterminism.
data ChoreoNode
  = CLeaf
  | CSeq ChoreoNode ChoreoNode
  | CChoice Choice
  | CAsync ChoreoNode ChoreoNode
  | CStep Step ChoreoNode
  deriving (Eq, Show)

-- | Flatten a choreography tree into a linear sequence of steps (for the simple engine).
flattenChoreo :: ChoreoNode -> Choreography
flattenChoreo CLeaf = []
flattenChoreo (CSeq a b) = flattenChoreo a ++ flattenChoreo b
flattenChoreo (CStep s rest) = s : flattenChoreo rest
flattenChoreo (CChoice c) = flattenChoreo (head (branches c))
flattenChoreo (CAsync a b) = flattenChoreo a ++ flattenChoreo b
