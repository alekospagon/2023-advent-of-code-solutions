import Data.List.Split
import qualified Data.Map as Map

-- read line -> returns #(winning numbers)
winning :: String -> Int
winning string = length [x | x<-l1, elem x l2]
        where
        lists = splitOn "|" string
        l1 = drop 2 $ map read $ words $ drop 2 $ lists!!0 :: [Int]
        l2 = map read $ words $ lists!!1 :: [Int]

-- generate copies from this card
card_update :: Int -> [Int] -> Map.Map Int Int -> Map.Map Int Int
card_update cardID [] m = m
card_update cardID (off:offs) copies_map = card_update cardID offs new_copies_map
        where
        copies = Map.findWithDefault 0 cardID copies_map
        new_copies_map = Map.insertWith (+) (cardID+off) (copies) copies_map


-- makes map of all cards
full_update :: Map.Map Int Int -> [Int] -> Map.Map Int Int -> Map.Map Int Int
full_update copies_map [] lengths = copies_map
full_update copies_map (i:is) lengths = full_update new_copies_map is lengths
        where
        next = Map.findWithDefault 0 i lengths
        offset = next
        new_copies_map = card_update i [1..offset] copies_map

-- makes map: [(ID, copies)]
make_copies :: Map.Map Int Int -> Map.Map Int Int
make_copies lengths = m_result
        where
        size = length lengths
        m_empty = Map.fromList $ take size $ zip [0..] (repeat 1)
        m_result = full_update m_empty [0..size] lengths


main :: IO ()
main = do
        input <- getContents
        let lengths = zip [0..] $ map winning $ lines input -- [(ID,winning)]
        let copies  = make_copies $ Map.fromList lengths -- [(ID, copies)]
        print $ sum $ Map.elems copies -- sum copies
