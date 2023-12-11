import Data.List

main = do
        line <- getLine
        let mapped = map (\x -> if x == '(' then 1 else -1) line
        putStrLn $ "PART 1: " ++ (show $ sum mapped)
        let prefix = length $ takeWhile (>=0) $ drop 1 . scanl (+) 0 $ mapped -- partialSums
        putStrLn $ "PART 2: " ++ (show $ 1 + prefix) -- 1-indexed
