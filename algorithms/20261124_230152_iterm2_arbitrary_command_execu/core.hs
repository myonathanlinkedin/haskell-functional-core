module Core (
    APC(..),
    parseAPC,
    renderAPC,
    isSafeCommand,
    sanitizeData
) where

import Data.Char (isSpace)

-- | Representation of an APC (Application Program Command) used in the Kitty
--   image protocol.  The command is a short identifier, the payload is an
--   arbitrary string (may be empty).
data APC = APC
    { command :: String
    , payload :: String
    } deriving (Eq, Show)

-- Escape character used by terminal control sequences.
escapeChar :: Char
escapeChar = '\ESC'   -- ASCII 27

-- APC delimiters according to the Kitty protocol.
startSeq :: String
startSeq = [escapeChar, '_']

endSeq :: String
endSeq = [escapeChar, '\\']

-- | Parse an APC from the beginning of a string.
--   Returns the APC and the remainder of the input after the terminating sequence.
parseAPC :: String -> Maybe (APC, String)
parseAPC s
    | not (startSeq `isPrefixOf` s) = Nothing
    | otherwise =
        let afterStart = drop (length startSeq) s
            (content, rest) = breakOn endSeq afterStart
        in if endSeq `isPrefixOf` rest
              then let apc = parseContent content
                       remaining = drop (length endSeq) rest
                   in Just (apc, remaining)
              else Nothing   -- no terminating sequence

-- | Render an APC back to its escaped representation.
renderAPC :: APC -> String
renderAPC (APC cmd pl) = startSeq ++ cmd ++ (if null pl then "" else " " ++ pl) ++ endSeq

-- | Determine whether a command is considered safe.
--   Only commands from the whitelist are allowed.
isSafeCommand :: String -> Bool
isSafeCommand cmd = cmd `elem` whitelist
  where
    whitelist = ["set", "reset", "clear"]

-- | Sanitize a string that may contain embedded APC sequences.
--   Unsafe APCs are removed entirely; safe APCs are preserved unchanged.
sanitizeData :: String -> String
sanitizeData "" = ""
sanitizeData s@(c:_)
    | startSeq `isPrefixOf` s =
        case parseAPC s of
            Just (apc, rest) ->
                if isSafeCommand (command apc)
                    then renderAPC apc ++ sanitizeData rest
                    else sanitizeData rest   -- drop unsafe APC
            Nothing -> c : sanitizeData (tail s)   -- malformed, skip one char
    | otherwise = c : sanitizeData (tail s)

-- Helper: check if a string is a prefix of another.
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Helper: break a string on the first occurrence of a pattern.
-- Returns (prefix, suffix) where suffix starts with the pattern (or empty if not found).
breakOn :: String -> String -> (String, String)
breakOn pat str = go "" str
  where
    go acc [] = (reverse acc, [])
    go acc rest@(r:rs)
        | pat `isPrefixOf` rest = (reverse acc, rest)
        | otherwise = go (r:acc) rs
