import qualified Data.List

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

convertToInts :: [String] -> [[Int]] -> [[Int]]
convertToInts [] elves =
  elves
convertToInts contents [] =
  convertToInts contents [[]]
convertToInts ("":contents) elves =
  convertToInts contents $ []:elves
convertToInts (line:contents) (elf:elves) =
  convertToInts contents $ ((read line) : elf):elves

sumElves :: [[Int]] -> [Int]
sumElves elves =
  map sum elves

main :: IO ()
main = do
  contents <- readLines ("input.txt")
  let elves = convertToInts contents []
      calElves = sumElves elves
      sortedElves = reverse $ Data.List.sort calElves
    in do
    print $ maximum calElves
    print $ sum $ take 3 sortedElves
