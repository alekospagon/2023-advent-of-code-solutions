import Data.List

main = do
	contents <- readFile "text"
	let nums = map read $ lines contents :: [Int]
	let tuple2 = head [x*y | 
		(x:ys)<-tails nums,
		y<-ys, x+y == 2020] :: Int
	let tuple3 = head [x*y*z | 
		(x:ys)<-tails nums, 
		(y:zs)<-tails ys, 
		z<- zs, x+y+z==2020] :: Int
	putStrLn $ "PART 1: " ++ (show tuple2)
	putStrLn $ "PART 2: " ++ (show tuple3)
