{-# LANGUAGE BangPatterns #-}

start = "1113122113"

step :: [Char] -> Char -> Int -> [Char]
step [] last !acc = (show acc) ++ [last]
step (a:xs) last !acc =
        if a == last
        then step xs last (1+acc)
        else (show acc) ++ [last] ++ (step xs a 1)

solve :: [Char] -> [Char]
solve lst = step (tail lst) (head lst) 1


main = do
        putStrLn $ "PART 1: " ++ (show $ length $ (iterate solve start) !! 40)
        putStrLn $ "PART 2: " ++ (show $ length $ (iterate solve start) !! 50)
