import Data.List

coords :: [String] -> [(Int,Int)]
coords input = [ (x,y) | (y,row) <- zip [1..] input, (x, char) <- zip [1..] row, char == '#']

expand :: [(Int,Int)] -> Int -> [(Int,Int)]
expand galaxies expansion = new_galaxies
        where
        max_x = maximum $ map fst galaxies
        max_y = maximum $ map snd galaxies
        empty_row = [y | y<-[1..max_y] \\ (map snd galaxies)]
        empty_col = [x | x<-[1..max_x] \\ (map fst galaxies)]
        new_galaxies = [(dx+x,dy+y) | (x,y) <- galaxies,
                let dx = (expansion-1) * length (filter (<x) empty_col),
                let dy = (expansion-1) * length (filter (<y) empty_row)]


manh :: ((Int,Int) , (Int,Int)) -> Int
manh ((x1,y1), (x2,y2)) = abs (x2-x1) + abs (y2-y1)

pairs :: [a] -> [(a,a)]
pairs coords = [(x,y) | (x:ys) <- tails coords, y <- ys]


main = do
        raw_input <- getContents
        let galaxies = coords $ lines raw_input
        let expanded1 = expand galaxies 2
        let expanded2 = expand galaxies 1000000
        putStrLn $ "PART 1: " ++ (show $ sum $ map manh $ pairs expanded1)
        putStrLn $ "PART 2: " ++ (show $ sum $ map manh $ pairs expanded2)
