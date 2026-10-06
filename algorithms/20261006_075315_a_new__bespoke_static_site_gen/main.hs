import Data.Char (isAlpha, isSpace)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let
    urlInput = "https://example.com/path?query#fragment"
    fragment = extractFragment urlInput

  putStrLn $ "Fragment: " ++ fragment
