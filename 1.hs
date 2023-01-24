import System.IO
import Data.Strings
import Data.List

main = do
  content <- readFile "input/1.txt"
  let rawBlocks = strSplitAll "\n\n" content
  let sums =  map (foldl (+) 0) $ map (makeAllInt . lines) rawBlocks
  print $ foldl (+) 0 $ take 3 $ reverse $ sort sums


makeAllInt :: [String] -> [Integer]
makeAllInt strs = map (read :: String -> Integer) strs