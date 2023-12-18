{-# LANGUAGE BangPatterns #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List


max_int = 10000000


neighbours :: Map (Int,Int) Char -> (Int,Int) -> Char -> [(Int,Int)]
neighbours grid (y,x) this = up ++ down ++ left ++ right
        where
        search = (\x -> Map.findWithDefault '*' x grid)
        up    = if elem (search (y-1,x)) "|F7" then
                if elem this "JL|S" then [(y-1,x)] else [] else []
        down  = if elem (search (y+1,x)) "|JL" then
                if elem this "7F|S" then [(y+1,x)] else [] else []
        left  = if elem (search (y,x-1)) "-FL" then
                if elem this "-7JS" then [(y,x-1)] else [] else []
        right = if elem (search (y,x+1)) "-7J" then
                if elem this "-FLS" then [(y,x+1)] else [] else []


-- subroutine -> updates distance of neighbours
update_dist :: Map (Int,Int) Char -> Map (Int,Int) Int -> Int -> [(Int,Int)] -> Map (Int,Int) Int
update_dist grid !dist so_far [] = dist
update_dist grid !dist so_far (n:ns) = update_dist grid (Map.adjust (min (so_far+1)) n dist) so_far ns


-- Dijkstra with distances, a Set of seen nodes and a queue
search :: Map (Int,Int) Char -> Map (Int,Int) Int -> Set (Int,Int) -> [(Int,Int)] -> Map (Int,Int) Int
search grid !dist !seen [] = dist -- done 
search grid !dist !seen queue = search grid new_dist new_seen new_queue
        where
        node   = head queue
        so_far = Map.findWithDefault (-1) node dist -- path up to this point
        neighs = neighbours grid node (Map.findWithDefault '*' node grid)
        new_dist  = update_dist grid dist so_far neighs -- update neighbour dists
        new_queue = (tail queue) ++ unseen -- pop and attach new
        new_seen  = Set.insert node seen -- now im done with node
        unseen    = filter (flip Set.notMember seen) neighs -- unseen   



main = do
        c <- getContents
        let grid  = Map.fromList [((y,x), c) | (y,l) <- zip [0..] (lines c), (x,c) <- zip [0..] l]
        let dist' = Map.fromList [((y,x), max_int) | (y,l) <- zip [0..] (lines c), (x,c) <- zip [0..] l]
        let entry_point = head $ filter (\x -> Map.findWithDefault '*' x grid == 'S') $ Map.keys grid
        let dist = Map.adjust (min 0) entry_point dist'
        let seen = Set.singleton entry_point -- entry point seen
        let dijkstra_dist = search grid dist seen [entry_point]
        let solution = maximum $ filter (< max_int) $ Map.elems dijkstra_dist
        print solution
