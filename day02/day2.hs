data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

rps :: RPS -> RPS -> Int
rps Scissors Rock  = 1 + 6
rps Rock Paper     = 2 + 6
rps Paper Scissors = 3 + 6
rps a b | a == Rock && a == b = 1 + 3
rps a b | a == Paper && a == b = 2 + 3
rps a b | a == Scissors && a == b = 3 + 3
rps _ Rock = 1
rps _ Paper = 2
rps _ Scissors = 3

win :: RPS -> RPS
win Rock     = Paper
win Paper    = Scissors
win Scissors = Rock

lose :: RPS -> RPS
lose Rock     = Scissors
lose Scissors = Paper
lose Paper    = Rock

convertToRps1 :: String -> Maybe RPS
convertToRps1 "A" = return Rock
convertToRps1 "B" = return Paper
convertToRps1 "C" = return Scissors
convertToRps1 "X" = return Rock
convertToRps1 "Y" = return Paper
convertToRps1 "Z" = return Scissors
convertToRps1 _   = Nothing

convertToRps2 :: String -> String -> String -> Maybe RPS
convertToRps2 p1 "X" player | player `elem` ["X","Y","Z"] = fmap lose $ convertToRps1 p1
convertToRps2 p1 "Y" player | player `elem` ["X","Y","Z"] = convertToRps1 p1
convertToRps2 p1 "Z" player | player `elem` ["X","Y","Z"] = fmap win $ convertToRps1 p1
convertToRps2 _ _ player = convertToRps1 player

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
  simpleContents <- readLines "inputSimple.txt"
  contents <- readLines "input.txt"

  simpleWordsList <- return $ mapM (\x -> mapM convertToRps1 $ words x) contents
  simpleScoreList <- return $ simpleWordsList >>= (\x -> return $ map (\y -> rps (y!!0) (y!!1)) x)
  simpleScore <- return $ simpleScoreList >>= (\x -> return (sum x))

  wordsList       <- return $ mapM (\x -> mapM (\y -> convertToRps2 (words x !! 0) (words x !! 1) y) $ words x) contents
  scoreList <- return $ wordsList >>= (\x -> return $ map (\y -> rps (y!!0) (y!!1)) x)
  score <- return $ scoreList >>= (\x -> return (sum x))

  --print simpleWordsList
  print simpleScore
  print score
