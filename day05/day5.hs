import Data.Char

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

stacksAndMoves :: [String] -> ([String], [String])
stacksAndMoves input =
  let f ("":xs) acc = (drop 1 acc, xs)
      f (x:xs) acc  = f xs (x:acc)
      f [] _        = undefined -- Should never happen
  in f input []

getStacks :: [String] -> [String]
getStacks input =
  let line [] = []
      line y  = (take 4 y) : (line $ drop 4 y)
      transpose [] = []
      transpose ([]:_) = []
      transpose x  = map head x : (transpose $ map tail x)
      allLines = map line input
  in map (filter ((/=) ' ') . reverse . map (head . drop 1)) $ transpose allLines

parseMove :: String -> [Int]
parseMove [] = []
parseMove x =
  let restString = dropWhile isDigit $ dropWhile (not . isDigit) x
  in (read $ takeWhile isDigit $ dropWhile (not . isDigit) x) : parseMove restString

getMoves :: [String] -> [(Int, Int, Int)]
getMoves []     = []
getMoves (x:xs) =
  let move      = parseMove x
      numMoves  = move !! 0
      from      = move !! 1 - 1
      to        = move !! 2 - 1
  in (numMoves, from, to) : getMoves xs

emptyStack :: [String] -> Int -> Int -> [String]
emptyStack stacks numMoves index =
  let helper (stack:stacks') currentStack | currentStack == index =
                                            (drop numMoves stack):stacks'
      helper (stack:stacks') currentStack = stack:(helper stacks' $ currentStack + 1)
      helper _ _ = undefined -- doesn't handle incorrect input
  in helper stacks 0

fillStack :: [String] -> String -> Int -> [String]
fillStack stacks crates index =
  let helper (stack:stacks') currentStack | currentStack == index =
                                            (crates ++ stack) : stacks'
      helper (stack:stacks') currentStack = stack:(helper stacks' $ currentStack + 1)
      helper _ _ = undefined -- doesn't handle incorrect input
  in helper stacks 0

moveStacks :: [String] -> [(Int, Int, Int)] -> [String]
moveStacks stacks [] = stacks
moveStacks stacks ((numMoves, from, to):moves) =
  let movedCrates = reverse $ take numMoves (stacks !! from)
      emptyStacks = emptyStack stacks numMoves from
      newStacks   = fillStack emptyStacks movedCrates to
  in moveStacks newStacks moves

moveStacksSmart :: [String] -> [(Int, Int, Int)] -> [String]
moveStacksSmart stacks [] = stacks
moveStacksSmart stacks ((numMoves, from, to):moves) =
  let movedCrates = take numMoves (stacks !! from)
      emptyStacks = emptyStack stacks numMoves from
      newStacks   = fillStack emptyStacks movedCrates to
  in moveStacksSmart newStacks moves

main :: IO ()
main = do
  contents <- readLines "input.txt"
  let (tempStacks, tempMoves) = stacksAndMoves contents
      stacks = getStacks tempStacks
      moves = getMoves tempMoves
  print $ moveStacks stacks moves
  print $ map head $ moveStacks stacks moves
  print $ map head $ moveStacksSmart stacks moves
