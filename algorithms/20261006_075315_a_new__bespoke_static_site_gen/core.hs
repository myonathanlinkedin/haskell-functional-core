import Data.Char (isAlpha, isSpace)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromJust)

-- Define a function to extract the fragment from a URL
extractFragment :: String -> String
extractFragment url =
  let
    trimmedUrl = dropWhile (`isSpace`) url
    parts = splitOn "#" trimmedUrl
    fragment = fromJust $ head parts
  in
    fragment
