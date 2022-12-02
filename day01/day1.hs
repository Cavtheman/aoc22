import qualified Data.List

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

splitter :: [Int] -> String -> [Int]
splitter [] _     = [0]
splitter acc ""   = 0:acc
splitter (x:xs) s = (x + read s) : xs

main :: IO ()
main = do
  simpleContents <- readLines ("inputSimple.txt")
  contents <- readLines ("input.txt")
  let simpleElves = Data.List.sortBy (flip compare) $ foldl splitter [] simpleContents
      elves = Data.List.sortBy (flip compare) $ foldl splitter [] contents
    in do
    print $ sum $ take 1 simpleElves
    print $ sum $ take 3 simpleElves

    print $ sum $ take 1 elves
    print $ sum $ take 3 elves
