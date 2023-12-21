import Data.List.Split

-- input handling. no need to keep track of map titles. just return them in order
parse input = (seeds, maps)
        where
        h:t = splitOn [""] $ lines input -- raw seeds and rest
        seeds = map read $ tail $ splitOn " " $ head h :: [Int]
        make_map (title:values) = (map.map) read $ map (splitOn " ") values :: [[Int]]
        maps  = map make_map t -- no zip


-- search a map
search [] num = num -- no map is ID map
search (x:xs) num = if (src <= num) && (num < src+rng) then dest + (num-src) else search xs num
        where [dest, src, rng] = x

-- search map by map    
solve maps num = foldl (flip search) num maps

main = do
        raw_input <- getContents
        let (seeds, maps) = parse raw_input
        let solutions = map (solve maps) seeds
        print $ minimum solutions
