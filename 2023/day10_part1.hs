import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

max_int = 100000000

neighbours grid (y,x) this = up ++ down ++ left ++ right
        where -- check where I can move *to* and *from* for all four directions
        valid coords to from = if elem (grid Map.! coords) to && elem this from then [coords] else []
        up    = valid (y-1,x) "|F7" "JL|S"
        down  = valid (y+1,x) "|JL" "7F|S"
        left  = valid (y,x-1) "-FL" "-7JS"
        right = valid (y,x+1) "-7J" "-FLS"

-- Dijkstra subroutine -> updates distance of neighbours
aux grid dist so_far [] = dist
aux grid dist so_far (n:ns) = aux grid (Map.adjust (min (so_far+1)) n dist) so_far ns

-- Dijkstra
search grid dist seen [] = dist -- done 
search grid dist seen (node:popped) = search grid new_dist new_seen new_queue
        where
        neighs   = neighbours grid node (grid Map.! node)
        new_dist  = aux grid dist (dist Map.! node) neighs
        new_seen  = Set.insert node seen
        new_queue = popped ++ filter (flip Set.notMember seen) neighs

-- make grid, make distances with infinity, then do Dijkstra
main = do
        c <- getContents
        let grid = Map.fromList [((y,x), c) | (y,l) <- zip [0..] (lines c), (x,c) <- zip [0..] l]
        let dist = Map.fromList [((y,x), if c == 'S' then 0 else max_int) |
                (y,l) <- zip [0..] (lines c), (x,c) <- zip [0..] l]
        let entry = head $ filter (\x -> grid Map.! x == 'S') $ Map.keys grid -- coords of 'S'
        print $ maximum $ filter (< max_int) $ Map.elems $ search grid dist (Set.singleton entry) [entry]
