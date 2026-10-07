module Core where

import Data.Char (isDigit)
import Text.Read (readMaybe)

-- | Representation of a program status report.
data ProgramStatus = ProgramStatus
  { psPid    :: Int    -- ^ Process identifier
  , psStatus :: String -- ^ Human readable status string
  } deriving (Eq, Show)

-- Control characters used by the OSC protocol.
escChar :: Char
escChar = '\ESC'   -- ASCII 27

belChar :: Char
belChar = '\BEL'   -- ASCII 7

-- | Encode a 'ProgramStatus' into an OSC 7501 string.
--   Format: ESC ] 7501 ; pid=<pid>;status=<status> BEL
encodeOSC7501 :: ProgramStatus -> String
encodeOSC7501 (ProgramStatus pid status) =
  escChar : ']' : "7501;" ++ "pid=" ++ show pid ++ ";status=" ++ escape status ++ belChar : []
  where
    -- Escape any BEL characters inside the status string.
    escape = concatMap (\c -> if c == belChar then "" else [c])

-- | Decode an OSC 7501 string into a 'ProgramStatus'.
--   Returns 'Nothing' if the string does not conform to the expected format.
decodeOSC7501 :: String -> Maybe ProgramStatus
decodeOSC7501 s = do
  rest1 <- stripPrefix [escChar,']',"7501;"] s
  let (payload, rest2) = break (== belChar) rest1
  guard (not (null rest2) && head rest2 == belChar)
  kvs <- parseKeyValues payload
  pidStr   <- lookup "pid" kvs
  statusStr<- lookup "status" kvs
  pid <- readMaybe pidStr
  return $ ProgramStatus pid statusStr

-- Helper: strip a prefix, returning the remainder if it matches.
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
  | x == y    = stripPrefix xs ys
  | otherwise = Nothing
stripPrefix _ _ = Nothing

-- Helper: guard for Maybe.
guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing

-- Parse a semicolon‑separated list of key=value pairs.
parseKeyValues :: String -> Maybe [(String,String)]
parseKeyValues "" = Just []
parseKeyValues str = sequence $ map parsePair (splitOn ';' str)

-- Split a string on a delimiter.
splitOn :: Char -> String -> [String]
splitOn delim = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == delim = []:acc
      | otherwise  = (c:x):xs
    f _ _ = error "Impossible"

-- Parse a single key=value pair.
parsePair :: String -> Maybe (String,String)
parsePair kv =
  case break (== '=') kv of
    (k, '=':v) -> Just (k,v)
    _          -> Nothing
