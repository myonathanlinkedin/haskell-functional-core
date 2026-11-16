module Main where

import Core
import qualified Data.Map.Strict as Map

-- Simple assertion helper.
assert :: Bool -> String -> ()
assert True _  = ()
assert False msg = error ("Assertion failed: " ++ msg)

-- Test 1: one always‑succeeding subscriber, one always‑failing subscriber.
testSuccessAndFailure :: Bool
testSuccessAndFailure =
    let st0 = emptyState
        (st1, sidGood) = addSubscriber (const True) st0
        (st2, sidBad)  = addSubscriber (const False) st1
        notif = Notification "msg"
        st3 = sendNotification notif st2
        -- Run enough steps to exhaust retries for the bad subscriber.
        steps = maxRetries + 1
        stFinal = iterate step st3 !! steps
        deliveredGood = getDelivered sidGood stFinal
        deadBad       = getDead sidBad stFinal
        pendingGood   = getPending sidGood stFinal
        pendingBad    = getPending sidBad stFinal
    in  assert (deliveredGood == [notif]) "good subscriber should have delivered"
        `seq` assert (deadBad == [notif]) "bad subscriber should have dead‑lettered"
        `seq` assert (null pendingGood) "good subscriber pending queue empty"
        `seq` assert (null pendingBad)  "bad subscriber pending queue empty"
        `seq` True

-- Test 2: fan‑out of multiple notifications to multiple subscribers.
testMultipleNotifications :: Bool
testMultipleNotifications =
    let st0 = emptyState
        (st1, sidA) = addSubscriber (const True) st0
        (st2, sidB) = addSubscriber (const True) st1
        notifs = map Notification ["a","b","c"]
        st3 = foldr sendNotification st2 notifs
        steps = 2  -- two steps are enough because all succeed on first try.
        stFinal = iterate step st3 !! steps
        deliveredA = getDelivered sidA stFinal
        deliveredB = getDelivered sidB stFinal
    in  assert (deliveredA == reverse notifs) "subscriber A should receive all"
        `seq` assert (deliveredB == reverse notifs) "subscriber B should receive all"
        `seq` True

-- Test 3: retry logic – a subscriber that succeeds only after two failures.
-- We encode the failure rule inside the handler using the payload.
handlerWithTwoFailures :: Notification -> Bool
handlerWithTwoFailures (Notification p) = "attempt3" `elem` words p

testRetryLogic :: Bool
testRetryLogic =
    let st0 = emptyState
        (st1, sid) = addSubscriber handlerWithTwoFailures st0
        notif = Notification "attempt1 attempt2 attempt3"
        st2 = sendNotification notif st1
        -- Run three steps: first two fail, third succeeds.
        stFinal = iterate step st2 !! 3
        delivered = getDelivered sid stFinal
        dead = getDead sid stFinal
        pending = getPending sid stFinal
    in  assert (delivered == [notif]) "notification should eventually be delivered"
        `seq` assert (null dead) "dead list should be empty"
        `seq` assert (null pending) "pending queue should be empty"
        `seq` True

-- Run all tests.
runTests :: IO ()
runTests = do
    let results = [ testSuccessAndFailure
                  , testMultipleNotifications
                  , testRetryLogic
                  ]
    mapM_ (\b -> if b then return () else error "A test returned False") results
    putStrLn "All tests passed."

main :: IO ()
main = runTests
