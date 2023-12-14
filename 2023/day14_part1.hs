import Data.List
import Data.List.Split


-- I dont have to move rocks. I just keep track of empty
-- spaces in ranges #...# and count rocks and then fill 
solve :: String -> [Int]
solve line = [sum $ take rocks empty | (empty,rocks) <- zip z_empty z_rocks]
        where
        l = length line
        -- split on # and zip with indices (counting from the right end as needed)
        z_ranges = splitWhen (\x -> '#' == (snd x)) [(l-i,c) | (i,c) <- zip [0..] line]
        z_empty  = (map.map) fst $ z_ranges -- keep only indices from each range
        z_rocks = map length $ map (filter ('O'==)) $ (map.map) snd z_ranges


main = do
        c <- getContents
        let grid = transpose $ lines c -- North becomes West
        let res  = map (sum . solve) grid
        print $ sum res
