import Data.List.Split

make_lists :: String -> ([Int], [Int])
make_lists string = (drop 2 l1, l2)
        where
        lists = splitOn "|" string
        l1 = map read $ words $ drop 2 $ lists!!0
        l2 = map read $ words $ lists!!1

score :: ([Int],[Int]) -> Int
score (l1, l2) = if common == 0 then 0 else 2^(common-1)
        where
        common = length [x | x<-l1, elem x l2]


main = do
        input <- getContents
        print $ sum $ map (score . make_lists) $ lines input
