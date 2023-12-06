import Data.List.Split
import qualified Data.Map as Map


parse_line :: String -> ([Int], [Int])
parse_line string = (drop 2 l1, l2)
        where
        lists = splitOn "|" string
        l1 = map read $ words $ drop 2 $ lists!!0
        l2 = map read $ words $ lists!!1

-- find winning number -> return len
winning :: ([Int], [Int]) -> Int
winning (l1, l2) = length [x | x<-l1, elem x l2]

-- updates copies for next "offset" cards
card_update start 0 m = m
card_update start offset copies_map = card_update start (offset-1) new_copies_map
        where
        copies = Map.findWithDefault 0 start copies_map
        new_copies_map = Map.insertWith (+) (start+offset) (copies) copies_map


-- makes map of all cards
full_update :: Map.Map Int Int -> Int -> Int -> Map.Map Int Int -> Map.Map Int Int
full_update copies_map it it_end lengths =
        if it == it_end then copies_map else full_update new_copies_map (it+1) it_end lengths
        where
        next = Map.findWithDefault 0 it lengths
        new_copies_map = card_update it next copies_map

-- makes map with copies
make_copies :: Map.Map Int Int -> Map.Map Int Int
make_copies lengths = m_result
        where
        size = length lengths
        m_empty = Map.fromList $ take size $ zip [0..] (repeat 1)
        m_result = full_update m_empty 0 size lengths



main = do
        input <- getContents
        let lengths = zip [0..] $ map (winning . parse_line) $ lines input
        let copies  = make_copies $ Map.fromList lengths
        print $ sum $ Map.elems copies
