import Data.List

solve1 x = floor (fromIntegral $ x `div` 3) - 2
solve2 x = sum $ tail $ takeWhile (>0) $ iterate solve1 x

main = do
        c <- getContents
        let nums = map read $ lines c :: [Int]
        print $ sum $ map solve1 nums
        print $ sum $ map solve2 nums
