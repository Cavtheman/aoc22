readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

isUnique :: String -> String -> Int -> Bool
isUnique _ _ 0 = True
isUnique (x:xs) seen n | notElem x seen = isUnique xs (x:seen) $ n - 1
isUnique _ _ _ = False

findAnyUnique :: Int -> String -> Maybe Int
findAnyUnique n s =
  let helper s' index | isUnique s' "" n = Just $ index + n
      helper (_:xs) index = helper xs $ index + 1
      helper _ _ = Nothing
  in helper s 0

main :: IO ()
main = do
  contents <- readLines "input.txt"
  let results1 = map (findAnyUnique 4) contents
      results2 = map (findAnyUnique 14) contents
  print results1
  print results2
