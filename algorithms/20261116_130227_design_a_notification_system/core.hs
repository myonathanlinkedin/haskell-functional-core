module Core (
    Notification(..),
    SystemState,
    emptyState,
    addSubscriber,
    sendNotification,
    step,
    getDelivered,
    getDead,
    getPending
) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Simple notification carrying a payload.
newtype Notification = Notification { payload :: String }
    deriving (Eq, Show)

-- | Internal per‑subscriber information.
data SubscriberInfo = SubscriberInfo
    { handler   :: Notification -> Bool          -- ^ Delivery attempt, True = success.
    , pending   :: [(Notification, Int)]         -- ^ Queue of (notification, retryCount).
    , delivered :: [Notification]                -- ^ Successfully delivered notifications.
    , dead      :: [Notification]                -- ^ Notifications that exceeded retry limit.
    }

-- | Global system state.
data SystemState = SystemState
    { subs   :: Map Int SubscriberInfo            -- ^ Map from subscriber id to its info.
    , nextId :: Int                               -- ^ Counter for generating fresh ids.
    }

-- | Maximum number of delivery attempts (initial try + retries).
maxRetries :: Int
maxRetries = 3

-- | Empty system state.
emptyState :: SystemState
emptyState = SystemState { subs = Map.empty, nextId = 0 }

-- | Add a new subscriber. Returns the updated state and the subscriber's identifier.
addSubscriber :: (Notification -> Bool) -> SystemState -> (SystemState, Int)
addSubscriber h st =
    let sid = nextId st
        info = SubscriberInfo { handler = h
                              , pending = []
                              , delivered = []
                              , dead = []
                              }
        newSubs = Map.insert sid info (subs st)
    in (st { subs = newSubs, nextId = sid + 1 }, sid)

-- | Enqueue a notification for *all* current subscribers (fan‑out).
sendNotification :: Notification -> SystemState -> SystemState
sendNotification n st =
    let upd info = info { pending = pending info ++ [(n, 0)] }
        newSubs = Map.map upd (subs st)
    in st { subs = newSubs }

-- | Process a single delivery step for every subscriber.
step :: SystemState -> SystemState
step st = st { subs = Map.map processSubscriber (subs st) }

-- | Helper: process the head of a subscriber's pending queue.
processSubscriber :: SubscriberInfo -> SubscriberInfo
processSubscriber info = case pending info of
    [] -> info
    ((n, r):rest) ->
        if handler info n
            then info { pending = rest, delivered = n : delivered info }
            else if r + 1 >= maxRetries
                then info { pending = rest, dead = n : dead info }
                else info { pending = rest ++ [(n, r + 1)], delivered = delivered info }

-- | Query helpers (pure).
getDelivered :: Int -> SystemState -> [Notification]
getDelivered sid st = maybe [] delivered (Map.lookup sid (subs st))

getDead :: Int -> SystemState -> [Notification]
getDead sid st = maybe [] dead (Map.lookup sid (subs st))

getPending :: Int -> SystemState -> [(Notification, Int)]
getPending sid st = maybe [] pending (Map.lookup sid (subs st))
