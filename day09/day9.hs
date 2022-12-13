import Data.Array
import Data.List
data Direction = U | D | L | R deriving (Show)

readLines :: FilePath -> IO [[String]]
readLines = fmap (map words . lines) . readFile

cToDir :: String -> Direction
cToDir "U" = U
cToDir "D" = D
cToDir "L" = L
cToDir "R" = R
cToDir _ = error "Invalid direction"

linesToCommands :: [[String]] -> [(Direction, Int)]
linesToCommands = map (\[x, y] -> (cToDir x, read y)) 

moveHead :: (Int, Int) -> Direction -> Int -> (Int, Int)
moveHead (x, y) U n = (x, y + 1)
moveHead (x, y) D n = (x, y - 1)
moveHead (x, y) L n = (x - 1, y)
moveHead (x, y) R n = (x + 1, y)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (tx, ty) (hx, hy) | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = (tx, ty)
                           | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))

allHeadMoves :: [(Direction, Int)] -> (Int, Int) -> [(Int, Int)]
allHeadMoves [] _ = []
allHeadMoves ((_,0):moves) start = allHeadMoves moves start
allHeadMoves ((dir, n):moves) start =
  let headMove = moveHead start dir n
  in headMove : allHeadMoves ((dir, n-1):moves) headMove

simTails :: [(Int, Int)] -> Int -> [(Int, Int)]
simTails headLocations 1 = drop 1 $ scanl moveTail (0,0) headLocations
simTails headLocations n = 
  let newHead = drop 1 $ scanl moveTail (0,0) headLocations
  in simTails newHead (n-1)

main :: IO ()
main = do
  contents <- readLines "input.txt"
  let commands = linesToCommands contents
  let headLocations = allHeadMoves commands (0,0)
  print $ length $ nub $ simTails headLocations 1 -- part 1
  print $ length $ nub $ simTails headLocations 9 -- part 2