import Data.List
import Data.List.Split
import Data.Map (fromList, (!))


beg s = 'A' == last s
end s = 'Z' == last s

-- solve for one key on map
count key cnt my_map (i:instructions) =
        if end key then cnt else
        if i == 'L'
                then count left  (1+cnt) my_map instructions
                else count right (1+cnt) my_map instructions
        where (left, right) = my_map ! key


-- part 2 --> just LCM of all
main = do
        instructions <- getLine
        _ <- getLine -- consume empty line
        rest <- getContents
        let my_map = fromList [(e!!0, (e!!1,e!!2)) | e<- map (words . filter (\e -> not (elem e ",()="))) $ lines rest]
        let starting_points = filter beg $ map (head . words) $ lines rest
        -- part 1 and 2
        print $ count "AAA" 0 my_map (cycle instructions)
        print $ foldr lcm 1 $ map (\x -> count x 0 my_map (cycle instructions)) starting_points
