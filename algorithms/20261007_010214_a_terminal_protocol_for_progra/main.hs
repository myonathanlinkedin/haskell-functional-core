module Main where

import Core
import System.Exit (exitFailure, exitSuccess)

-- Simple test framework -----------------------------------------------------

type Test = (String, Bool)

runTest :: Test -> IO Bool
runTest (msg, result) = do
  putStr $ msg ++ ": "
  if result
    then putStrLn "PASS" >> return True
    else putStrLn "FAIL" >> return False

runTests :: [Test] -> IO Bool
runTests ts = all id <$> mapM runTest ts

-- Test cases ---------------------------------------------------------------

testEncodeDecode :: Bool
testEncodeDecode =
  let ps = ProgramStatus 1234 "Running"
      encoded = encodeOSC7501 ps
  in decodeOSC7501 encoded == Just ps

testMalformedMissingEsc :: Bool
testMalformedMissingEsc =
  let bad = "]7501;pid=1;status=OK" ++ [belChar]
  in decodeOSC7501 bad == Nothing

testMalformedMissingBel :: Bool
testMalformedMissingBel =
  let bad = [escChar] ++ "]7501;pid=1;status=OK"
  in decodeOSC7501 bad == Nothing

testInvalidPid :: Bool
testInvalidPid =
  let bad = [escChar] ++ "]7501;pid=abc;status=OK" ++ [belChar]
  in decodeOSC7501 bad == Nothing

testEscapeInStatus :: Bool
testEscapeInStatus =
  let ps = ProgramStatus 42 "Line1\BELLine2"
      encoded = encodeOSC7501 ps
      -- The encoder strips BEL from status, so decoding should yield stripped status.
      expected = ProgramStatus 42 "Line1Line2"
  in decodeOSC7501 encoded == Just expected

allTests :: [Test]
allTests =
  [ ("Encode/Decode round‑trip", testEncodeDecode)
  , ("Missing ESC character", testMalformedMissingEsc)
  , ("Missing BEL terminator", testMalformedMissingBel)
  , ("Invalid PID value", testInvalidPid)
  , ("Status with BEL stripped", testEscapeInStatus)
  ]

-- Entry point --------------------------------------------------------------

main :: IO ()
main = do
  allPassed <- runTests allTests
  if allPassed
    then putStrLn "All tests passed." >> exitSuccess
    else putStrLn "Some tests failed." >> exitFailure

-- Helper: expose BEL character for tests (imported from Core)
belChar :: Char
belChar = '\BEL'   -- duplicate definition to avoid import cycles in tests.
