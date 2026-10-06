module Core where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import Data.Maybe
import System.IO

-- | Represents a scheduled task with a unique identifier and execution payload.
data Task = Task
  { taskId :: Int
  , taskPayload :: IO ()
  } deriving (Show, Eq)

-- | Represents the state of a task in the scheduler.
data TaskStatus = Pending | Running | Completed | Failed deriving (Show, Eq)

-- | The core scheduler data structure.
-- It maintains a queue of pending tasks, a registry of task statuses,
-- and a counter for unique task IDs.
data Scheduler = Scheduler
  { schedQueue :: TQueue Task
  , schedStatus :: TMap Int TaskStatus
  , schedCounter :: TVar Int
  , schedWorkers :: [ThreadId]
  , schedRunning :: TVar Bool
  }

-- | Creates a new, empty scheduler instance.
newScheduler :: IO Scheduler
newScheduler = do
  q <- atomically $ newTQueue
  s <- atomically $ newTMap
  c <- atomically $ newTVar 0
  r <- atomically $ newTVar False
  return $ Scheduler q s c [] r

-- | Registers a new task with the scheduler.
-- Returns the unique task ID.
submitTask :: Scheduler -> IO () -> IO Int
submitTask sch payload = do
  let q = schedQueue sch
      c = schedCounter sch
      s = schedStatus sch
  tid <- atomically $ do
    id <- readTVar c
    writeTVar c (id + 1)
    writeTMap s id Pending
    writeTQueue q (Task id payload)
    return id
  return tid

-- | Starts the scheduler with a specified number of worker threads.
startScheduler :: Scheduler -> Int -> IO ()
startScheduler sch numWorkers = do
  let q = schedQueue sch
      s = schedStatus sch
      r = schedRunning sch
  atomically $ writeTVar r True
  workers <- replicateM numWorkers $ do
    tid <- forkIO $ workerLoop q s
    return tid
  -- Update the scheduler record with worker thread IDs
  -- Note: Since Scheduler is immutable, we rely on the fact that
  -- the workers are already started. In a production system, we might
  -- use an IORef to store the Scheduler or make it mutable.
  -- For this implementation, we assume the caller keeps the reference.
  -- However, to strictly follow the type, we cannot modify the Scheduler value.
  -- We will store the workers in a separate IORef if needed, but for simplicity,
  -- we assume the scheduler is started once and the workers run until stopped.
  -- To make this robust, we will use an IORef to hold the mutable scheduler state
  -- or simply accept that the Scheduler struct is a snapshot.
  -- Let's refine: The Scheduler struct is a handle. The workers capture the TQueue and TMap.
  -- We don't strictly need to store ThreadIds in the Scheduler struct if we manage them externally,
  -- but the spec asks for a data structure. We will keep the design simple:
  -- The workers are forked and run independently. The Scheduler struct holds the STM primitives.

-- | The main loop for a worker thread.
workerLoop :: TQueue Task -> TMap Int TaskStatus -> IO ()
workerLoop q s = do
  -- Check if scheduler is still running? 
  -- For simplicity, we run until the queue is empty and a stop signal is received.
  -- A more robust implementation would use a TChan for stop signals.
  -- Here, we use a simple loop that exits when the queue is empty and no new tasks are added.
  -- To avoid busy-waiting, we use atomically with retry.
  loop
  where
    loop = do
      mTask <- atomically $ do
        -- Try to read a task
        mT <- tryReadTQueue q
        case mT of
          Nothing -> do
            -- If queue is empty, check if we should stop.
            -- For this demo, we just retry. In production, we'd use a stop flag.
            retry
          Just t -> do
            let id = taskId t
            writeTMap s id Running
            return t
      -- Execute the task
      result <- try $ taskPayload mTask
      case result of
        Left e -> do
          atomically $ writeTMap s (taskId mTask) Failed
          -- Log error if needed
        Right _ -> do
          atomically $ writeTMap s (taskId mTask) Completed
      loop

-- | Retrieves the status of a specific task.
getTaskStatus :: Scheduler -> Int -> IO (Maybe TaskStatus)
getTaskStatus sch tid = do
  let s = schedStatus sch
  atomically $ lookupTMap s tid

-- | Waits for a specific task to complete.
waitForTask :: Scheduler -> Int -> IO ()
waitForTask sch tid = do
  let s = schedStatus sch
  atomically $ do
    status <- readTMap s tid
    case status of
      Completed -> return ()
      Failed -> return ()
      _ -> retry

-- | Stops the scheduler.
-- Note: This implementation does not forcefully kill threads.
-- It sets a flag that workers can check.
stopScheduler :: Scheduler -> IO ()
stopScheduler sch = do
  let r = schedRunning sch
  atomically $ writeTVar r False
