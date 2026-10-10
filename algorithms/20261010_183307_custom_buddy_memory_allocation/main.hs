module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

-- Simple test harness ----------------------------------------------------
runTest :: String -> Bool -> IO ()
runTest name cond = do
  putStrLn $ name ++ ": " ++ (if cond then "PASS" else "FAIL")
  if cond then return () else exitFailure

-- -----------------------------------------------------------------------
main :: IO ()
main = do
  let bud0 = initBuddy 1024

  -- Allocate 100 bytes (needs a 128‑byte block)
  case allocate bud0 100 of
    Nothing -> runTest "Allocate 100 (should succeed)" False
    Just (off1, bud1) -> do
      runTest "Allocate 100 offset" (off1 == 0)

      -- Allocate 200 bytes (needs a 256‑byte block)
      case allocate bud1 200 of
        Nothing -> runTest "Allocate 200 (should succeed)" False
        Just (off2, bud2) -> do
          runTest "Allocate 200 offset" (off2 == 128)

          -- Free the first block (128 bytes)
          let bud3 = free bud2 off1 128
          runTest "Free first block" True

          -- Free the second block (256 bytes)
          let bud4 = free bud3 off2 256
          runTest "Free second block" True

          -- After both frees, the allocator should have coalesced to a single
          -- free block covering the whole region.
          runTest "Full coalescence" (freeList bud4 == [(0,1024)])

          -- Attempt to allocate more than total size (should fail)
          case allocate bud4 2048 of
            Nothing -> runTest "Allocate too large (expected fail)" True
            Just _  -> runTest "Allocate too large (unexpected success)" False

          -- Allocate the whole memory at once
          case allocate bud4 1024 of
            Nothing -> runTest "Allocate full size (should succeed)" False
            Just (offFull, budFull) -> do
              runTest "Allocate full offset" (offFull == 0)
              runTest "Free full block" True
              let budAfterFree = free budFull offFull 1024
              runTest "Back to initial state" (freeList budAfterFree == [(0,1024)])

              -- Stress test: allocate many small blocks and free them
              let allocateMany b 0 acc = (b, reverse acc)
                  allocateMany b n acc =
                    case allocate b 1 of
                      Nothing -> (b, reverse acc)  -- should never happen
                      Just (off, b') -> allocateMany b' (n-1) ((off,1):acc)
              let (budMany, offs) = allocateMany budAfterFree 16 []
              runTest "Allocate 16 one‑byte blocks" (length offs == 16)
              let budFreed = foldr (\(o,_) acc -> free acc o 1) budMany offs
              runTest "Free all one‑byte blocks" (freeList budFreed == [(0,1024)])

              exitSuccess

// End of file.
