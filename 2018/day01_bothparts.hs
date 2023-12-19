import Data.Set hiding (map)

dup set (x:xs) = if member x set then x else dup (insert x set) xs

skip lst = if head lst == '+' then tail lst else lst

main = do
        c <- getContents
        let freqs = map (read . skip) $ lines c
        let endless = freqs ++ endless
        let part_sums = scanl (+) 0 endless
        putStrLn $ "PART 1: " ++ (show $ sum freqs)
        putStrLn $ "PART 2: " ++ (show $ dup empty part_sums)
