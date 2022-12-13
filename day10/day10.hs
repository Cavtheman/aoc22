readLines :: FilePath -> IO [[String]]
readLines = fmap (map words . lines) . readFile

performOps :: [Maybe Int] -> Maybe Int -> Int -> [Int]
performOps (op:ops) Nothing acc = 
    acc : performOps ops op acc 
performOps ops (Just finishOp) acc =
    let newAcc = acc + finishOp
    in acc : performOps ops Nothing newAcc
performOps [] _ _ = []

strToOps :: [[String]] -> [Maybe Int]
strToOps = map (\x -> if x !! 0 == "noop" then Nothing else Just (read (x !! 1)))

drawCRT :: [Int] -> Int -> String
drawCRT register 240 = []
drawCRT (r:register) i | abs (r - (mod i 40)) <= 1 = '#' : drawCRT register (i + 1)
                       | otherwise = ' ' : drawCRT register (i + 1)

cutLines :: String -> [String]
cutLines [] = []
cutLines s = take 40 s : cutLines (drop 40 s)

drawLines :: [String] -> IO ()
drawLines [] = return ()
drawLines (l:ls) = do
    print l
    drawLines ls

main :: IO ()
main = do
    contents <- readLines "input.txt"
    let ops = strToOps contents
        register = performOps ops Nothing 1
    print $ take 240 register
    print $ sum [register !! (i-1) * i | i <- [20,60..220]] -- part 1
    drawLines $ cutLines $ drawCRT register 0 -- part 2