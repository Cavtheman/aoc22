readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

getRanges :: String -> ((Int, Int), (Int, Int))
getRanges s =
  let (x, y) = break ((==) ',') s
      (x', x'') = break ((==) '-') x
      (y', y'') = break ((==) '-') $ drop 1 y
  in ((read x', read $ drop 1 x''), (read y', read $ drop 1 y''))

rangeContains :: (Int, Int) -> (Int, Int) -> Bool
rangeContains (startx, stopx) (starty, stopy) =
  (startx <= starty && stopx >= stopy) || (starty <= startx && stopy >= stopx)

rangeOverlaps :: (Int, Int) -> (Int, Int) -> Bool
rangeOverlaps (startx, stopx) (starty, stopy) =
  let maxStart = max startx starty
      minStop  = min stopx stopy
  in maxStart <= minStop

main :: IO ()
main = do
  contents <- readLines "input.txt"
  let allRanges = map getRanges contents
      allContains = map (\(x, y) -> rangeContains x y) allRanges
      allOverlaps = map (\(x, y) -> rangeOverlaps x y) allRanges
  print $ sum $ map fromEnum allContains
  print $ sum $ map fromEnum allOverlaps
