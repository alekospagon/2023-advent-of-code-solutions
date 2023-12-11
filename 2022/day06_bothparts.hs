import Data.List

solve :: String -> Int -> Int
solve lst len = aux 0 lst
        where
        aux cnt lst = if dist == len then (cnt+len) else aux (1+cnt) (tail lst)
                where
                dist = length $ nub $ take len lst

main = do
        input <- getLine
        putStrLn $ "PART 1: " ++ (show $ solve input 4)
        putStrLn $ "PART 2: " ++ (show $ solve input 14)
