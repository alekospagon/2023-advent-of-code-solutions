import Data.List

splitList :: [a] -> ([a],[a])
splitList [] = ([],[])
splitList (a:b:rest) = (a:odds, b:evens)
        where
        (odds, evens) = splitList rest


coords :: (Int,Int) -> [Char] -> [(Int,Int)]
coords (x,y) [] = [(x,y)]
coords (x,y) ('^':cs) = (x,y):(coords (x,y-1) cs)
coords (x,y) ('v':cs) = (x,y):(coords (x,y+1) cs)
coords (x,y) ('<':cs) = (x,y):(coords (x-1,y) cs)
coords (x,y) ('>':cs) = (x,y):(coords (x+1,y) cs)
coords (x,y) _ = [(x,y)] -- default case, just in case ...


main = do
        line <- getLine
        let (h1,h2) = splitList line
        let houses1  = coords (0,0) line
        let houses2  = (coords (0,0) h1) ++ (coords (0,0) h2)
        let unique1  = nub houses1 -- removes duplicates
        let unique2  = nub houses2
        putStrLn $ "PART 1: " ++ (show $ length unique1)
        putStrLn $ "PART 2: " ++ (show $ length unique2)
