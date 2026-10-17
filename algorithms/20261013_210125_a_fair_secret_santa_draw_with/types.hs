module Types where

-- | A participant is identified by a name.
type Participant = String

-- | An exclusion pair (giver, receiver) indicates that the giver
--   is not allowed to give a gift to the receiver.
type Exclusion = (Participant, Participant)

-- | An assignment is a list of (giver, receiver) pairs.
type Assignment = [(Participant, Participant)]
