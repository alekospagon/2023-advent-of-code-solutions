import Data.List.Split

parse :: String -> [Int]
parse line = map read $ splitOn "x" line


papper [d1,d2,d3] = 2*d1*d2 + 2*d1*d3 + 2*d2*d3 + m
        where
        m = minimum [d1*d2,d1*d3,d2*d3]

ribbon dimensions = 2 * ((sum dimensions) - (maximum dimensions))

bow dimensions = foldr (*) 1 dimensions

main = do
        c <- getContents
        let raw_cases = lines c
        let cases = map parse raw_cases
        let part1 = sum $ map papper cases
        let part2a = sum $ map ribbon cases
        let part2b = sum $ map bow cases
        putStrLn $ "PART 1: " ++ (show part1)
        putStrLn $ "PART 2: " ++ (show $ part2a + part2b)
