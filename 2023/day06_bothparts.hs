ways :: (Int,Int) -> Int
ways (time,dist) = - (floor t2 :: Int) + (ceiling t1 :: Int) - 1
	where -- second order polynomial inequality
	a = -1.0
	b = fromIntegral time
	c = - fromIntegral dist
	det = b^2 - 4 * a * c
	t1 = (-b - sqrt(det)) / (2*a)
	t2 = (-b + sqrt(det)) / (2*a)
	
-- toss "Time:" and "Distance:", map to ints and foldr ways on zipped
solve1 :: String -> String -> Int
solve1 times distances = total
	where 
	t = map read (tail (words times)) :: [Int]
	d = map read (tail (words distances)) :: [Int]
	total = foldr (*) 1 $ map ways (zip t d)

-- toss "Time:" and "Distance:", join to one int and call ways
solve2 :: String -> String -> Int
solve2 times distances = ways (t, d)
	where
	t = read $ foldr (++) "" $ tail $ words $ times :: Int
	d = read $ foldr (++) "" $ tail $ words $ distances :: Int

main :: IO ()
main = do
	times <- getLine
	distances <- getLine
	print $ solve2 times distances
