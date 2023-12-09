make_triangles :: [Int] -> [[Int]]
make_triangles [0] = [[0]]
make_triangles lst = lst:(make_triangles this)
        where
        this = zipWith (-) (tail lst) (init lst)



main = do
        raw <- getContents
        let integers = map (map (read :: String->Int) . words) $ lines raw :: [[Int]]
        let triangles = map make_triangles integers -- [  [a,b,c] , [d,e] , [z] ]
        print $ sum $ map (sum . (map last)) triangles -- add lasts of every triangle-list
