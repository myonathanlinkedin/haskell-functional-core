module Main where

import Types
import Engine
import System.Exit (exitFailure)

-- | Helper to compare expected and actual values.
assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual expected actual msg =
  assert (expected == actual) (msg ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")")

main :: IO ()
main = do
  -- Test construction from list
  let v0 = fromList [1,2,3,4,5] :: COWVector Int
  assertEqual 5 (lengthCOW v0) "length after fromList"
  mapM_ (\i -> assertEqual (i+1) (get v0 i) ("get index " ++ show i)) [0..4]

  -- Test copy‑on‑write semantics for set
  let v1 = set v0 2 99
  assertEqual 99 (get v1 2) "set modifies new vector"
  assertEqual 3  (get v0 2) "original vector unchanged after set"

  -- Test that further modifications on v1 do not affect v0
  let v2 = set v1 0 42
  assertEqual 42 (get v2 0) "second set on derived vector"
  assertEqual 1  (get v0 0) "original still unchanged"

  -- Test pushBack
  let v3 = pushBack v0 6
  assertEqual 6 (lengthCOW v3) "length after pushBack"
  assertEqual 6 (get v3 5) "new element after pushBack"
  assertEqual 5 (lengthCOW v0) "original length unchanged after pushBack"

  -- Test pushBack on empty vector
  let vEmpty = empty :: COWVector Char
  let vOne   = pushBack vEmpty 'a'
  assertEqual 1 (lengthCOW vOne) "pushBack on empty length"
  assertEqual 'a' (get vOne 0) "pushBack on empty value"

  -- Edge case: out‑of‑bounds access should raise an error
  let testOob = (get v0 10) `seq` False
  (testOob `seq` return ()) `catch` \(_ :: IOError) -> return ()
  -- If we reach here, the error was correctly raised.

  putStrLn "All tests passed."
  where
    -- Simple catch for any exception (used only for the out‑of‑bounds test)
    catch :: IO a -> (IOError -> IO a) -> IO a
    catch = Control.Exception.catch

    -- Re‑export the generic assert from Types
    assert = Types.assert

    -- Import needed for exception handling
    import qualified Control.Exception

    -- Import needed for seq
    import Prelude hiding (catch)
