import Data.List
import Data.Char (digitToInt)

myTakeWhile c [] = 0
myTakeWhile c (x:xs) = if x>=c then 1 else 1 + myTakeWhile c xs

aux grid gridT (y,x) = (c, left, right, up, down)
        where
        row = grid!!y
        column = gridT!!x
        c = row!!x
        left  = take x row
        right = drop (x+1) row
        up    = take y column
        down  = drop (y+1) column

is_visible grid gridT (y,x) = or $ map (all (c>)) [l, r, u, d]
        where (c, l, r, u, d) = aux grid gridT (y,x)

score grid gridT (y,x) = foldr (*) 1 $ map (myTakeWhile c) [reverse l, r, reverse u, d]
        where (c, l,r,u,d) = aux grid gridT (y,x)

main = do
        input <- getContents
        let grid = (map.map) digitToInt $ lines input :: [[Int]]
        let gridT = transpose grid
        let (max_y, max_x) = (length grid, length gridT)
        let trees = [(y,x) | x<-[1..(max_x-2)], y<-[1..(max_y-2)]] -- inner trees
        let inner = length $ filter (True==) $ map (is_visible grid gridT) trees
        let outer = 2*max_y + 2*max_x - 4 -- 4 edges are being counted twice
        putStrLn $ "PART 1: " ++ (show $ inner + outer)
        putStrLn $ "PART 2: " ++ (show $ maximum $ map (score grid gridT) trees)
