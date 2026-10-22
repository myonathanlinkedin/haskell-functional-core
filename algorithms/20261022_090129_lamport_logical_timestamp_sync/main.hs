module Main where

import Core

-- Simple assertion helper that aborts with a message on failure.
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test suite exercising all core operations and edge cases.
runTests :: IO ()
runTests = do
    -- Initialise two independent clocks (simulating two processes).
    let pA0 = initClock
    let pB0 = initClock

    assert (getTimestamp pA0 == 0) "initClock A"
    assert (getTimestamp pB0 == 0) "initClock B"

    -- Local events on A.
    let pA1 = tick pA0
    assert (getTimestamp pA1 == 1) "tick A1"

    let pA2 = tick pA1
    assert (getTimestamp pA2 == 2) "tick A2"

    -- A sends a message to B.
    let (pA3, tsMsg) = sendEvent pA2
    assert (getTimestamp pA3 == 3) "sendEvent A increments"
    assert (tsMsg == 3) "sendEvent timestamp attached"

    -- B receives the message.
    let pB1 = receiveEvent pB0 tsMsg
    assert (getTimestamp pB1 == 4) "receiveEvent B updates from 0 with 3"

    -- B performs a local event.
    let pB2 = tick pB1
    assert (getTimestamp pB2 == 5) "tick B after receive"

    -- B sends a message back to A with its current timestamp.
    let (pB3, tsReply) = sendEvent pB2
    assert (getTimestamp pB3 == 6) "sendEvent B increments"
    assert (tsReply == 6) "reply timestamp attached"

    -- A receives the reply.
    let pA4 = receiveEvent pA3 tsReply
    assert (getTimestamp pA4 == 7) "receiveEvent A updates from 3 with 6"

    -- Edge case: receiving a message with a timestamp far behind.
    let pA5 = receiveEvent pA4 1
    assert (getTimestamp pA5 == 8) "receiveEvent with smaller timestamp increments by 1"

    -- Edge case: multiple consecutive receives.
    let pA6 = receiveEvent pA5 20
    assert (getTimestamp pA6 == 21) "receiveEvent with much larger timestamp"

    putStrLn "All Lamport clock tests passed."

main :: IO ()
main = runTests
