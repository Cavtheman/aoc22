import Data.Char

readLines :: FilePath -> IO [[Int]]
readLines = fmap (map (map digitToInt) . lines) . readFile

main :: IO ()
main = do
  content <- readLines "inputSimple.txt"
  print content
