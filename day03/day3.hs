import Data.List
--import Data.List.Extra
import Data.Maybe

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

priority :: Char -> Int
priority c = fromMaybe (-1) $ elemIndex c alphabet >>= (\x -> return $ x + 1)

halveList :: [a] -> ([a], [a])
halveList l =
  let llength = length l
  in (take (llength `div` 2) l, drop (llength `div` 2) l)

makeBags :: [String] -> [([Int],[Int])]
makeBags l = map (halveList . map (priority)) l

findEquals :: [Int] -> [Int] -> Int
findEquals (x:_) s2 | elem x s2 = x
findEquals (_:xs) s2 = findEquals xs s2
findEquals _ _ = -100 -- I should use the Maybe type but I really can't be assed

findEquals2 :: [Int] -> [Int] -> [Int] -> Int
findEquals2 (x:_) s2 s3 | elem x s2 && elem x s3 = x
findEquals2 (_:xs) s2 s3 = findEquals2 xs s2 s3
findEquals2 _ _ _ = -100 -- I should use the Maybe type but I really can't be assed

getBadges :: [[Int]] -> [Int]
getBadges [] = []
getBadges bags =
  let bagGroup = take 3 bags
      restBags = drop 3 bags
      badge    = findEquals2 (bagGroup!!0) (bagGroup!!1) (bagGroup!!2)
  in badge:(getBadges restBags)

main :: IO ()
main = do
  contents <- readLines "input.txt"
  bags <- return $ makeBags contents
  bags2 <- return $ map (map priority) contents
  equals <- return $ map (\(x, y) -> findEquals x y) bags
  badges <- return $ getBadges bags2
  print $ sum equals
  print $ sum badges
