import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

max_int = 100000000

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
aux grid dist so_far [] = dist
aux grid dist so_far (n:ns) = aux grid (Map.adjust (min (so_far+1)) n dist) so_far ns

-- Dijkstra with distances, a Set of seen nodes and a queue
search grid dist seen [] = dist -- done 
search grid dist seen (node:popped) = search grid new_dist new_seen new_queue
        where
        neighs = neighbours grid node (grid Map.! node)
        new_dist  = aux grid dist (dist Map.! node) neighs 
        new_seen  = Set.insert node seen
        new_queue = popped ++ filter (flip Set.notMember seen) neighs 


main = do
        c <- getContents
        let grid = Map.fromList [((y,x), c) | (y,l) <- zip [0..] (lines c), (x,c) <- zip [0..] l]
        let dist = Map.fromList [((y,x), if c == 'S' then 0 else max_int) |
                (y,l) <- zip [0..] (lines c), (x,c) <- zip [0..] l]
        let entry = head $ filter (\x -> Map.findWithDefault '*' x grid == 'S') $ Map.keys grid
        print $ maximum $ filter (< max_int) $ Map.elems $ search grid dist (Set.singleton entry) [entry]
