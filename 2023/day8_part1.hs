{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.List.Split
import Data.Map (fromList, (!))


count "ZZZ" !cnt my_map (i:instructions) = cnt
count  key  !cnt my_map (i:instructions) =
        if i == 'L'
                then count left  (1+cnt) my_map instructions
                else count right (1+cnt) my_map instructions
        where (left, right) = my_map ! key



main = do
        instructions <- getLine
        _ <- getLine -- consume empty line
        rest <- getContents
        let my_map = fromList [(e!!0, (e!!1,e!!2)) | e<- map (words . filter (\e -> not (elem e ",()="))) $ lines rest]
        let gen = instructions ++ gen -- generate instructions ad infinitum
        print $ count "AAA" 0 my_map generator
