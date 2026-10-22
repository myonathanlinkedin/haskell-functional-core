module Core (
    LamportClock,
    initClock,
    tick,
    sendEvent,
    receiveEvent,
    getTimestamp
) where

-- | Pure representation of a Lamport logical clock.
newtype LamportClock = LamportClock { timestamp :: Int }
    deriving (Eq, Show)

-- | Initialise a clock with timestamp 0.
initClock :: LamportClock
initClock = LamportClock 0

-- | Increment the clock for a local event.
tick :: LamportClock -> LamportClock
tick (LamportClock t) = LamportClock (t + 1)

-- | Record a send event: increment the clock and return the new clock together
-- with the timestamp that will be attached to the outgoing message.
sendEvent :: LamportClock -> (LamportClock, Int)
sendEvent clk =
    let clk' = tick clk
    in (clk', getTimestamp clk')

-- | Record a receive event: update the clock using the received timestamp.
-- The new timestamp is max(local+1, received+1) to preserve causality.
receiveEvent :: LamportClock -> Int -> LamportClock
receiveEvent (LamportClock t) recvTs =
    LamportClock (max (t + 1) (recvTs + 1))

-- | Extract the current timestamp.
getTimestamp :: LamportClock -> Int
getTimestamp = timestamp
