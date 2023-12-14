import Data.List
import Data.List.Split

(part1, part2) = (0,1)

-- accumulate errors from every pair of strings and checks if its a valid mirroring
check lst = (sum $ map (\x -> diff (fst x) (snd x)) lst) == part2

-- accumulate errors from two different strings
diff lst1 lst2 = sum $ map (\(x,y) -> if x==y then 0 else 1) $ zip lst1 lst2

-- takes a mirroring and returns index if it is valid
flips lst = sum $ map (\(i,l1,l2) -> if check (zip (reverse l1) l2) then i else 0) lst

-- returns row indexes of all valid mirrorings
rows grid = flips [(i, take i grid, drop i grid) | i <- [1..((length grid)-1)]]
columns grid = rows (transpose grid) -- transpose A_ij->A_ji

main = do
        c <- getContents
        let grids = splitOn [""] $ lines c
        print $ sum $ map (\x -> (rows x) * 100 + (columns x)) grids
