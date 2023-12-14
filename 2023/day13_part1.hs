import Data.List
import Data.List.Split


-- checks if it is mirrored
check :: [([Char],[Char])] -> Bool
check [] = True
check ((x,y):xs) = if x == y then check xs else False


-- takes all splits of list and returns row_idx's when there is a mirror
flips :: [(Int,[[Char]],[[Char]])] -> Int
flips [] = 0
flips ((idx,lst1,lst2):rest) = this + (flips rest)
	where
	this = if check (zip (reverse lst1) lst2) then idx else 0


-- returns row indexes of mirrors
row :: [[Char]] -> Int
row grid = flips splits
	where
	splits = [(i, take i grid, drop i grid) | i <- [1..((length grid)-1)]]

	
-- transpose is built-in. does A_ij->A_ji
column grid = row (transpose grid)	


solve grid = r * 100 + c
	where
	r = row grid
	c = column grid


main = do
	c <- getContents
	let cases = splitOn [""] $ lines c
	let solutions = map solve cases
	print $ sum solutions
