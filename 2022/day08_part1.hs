{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Char (digitToInt)

visible grid gridT (y,x) = or $ map (all (c>)) [left, right, up, down]
        where
        row = grid!!y
        column = gridT!!x
        c = row!!x
        left  = take x row
        right = drop (x+1) row
        up    = take y column
        down  = drop (y+1) column

main = do
        input <- getContents
        let grid = (map.map) digitToInt $ lines input :: [[Int]]
        let gridT = transpose grid
        let max_y = length grid
        let max_x = length gridT
        let trees = [(y,x) | x<-[1..(max_x-2)], y<-[1..(max_y-2)]]
        let inner = length $ filter (True==) $ map (visible grid gridT) trees
        let outer = 2*max_y + 2*max_x - 4 -- 4 edges are being counted twice
        print $ inner + outer
