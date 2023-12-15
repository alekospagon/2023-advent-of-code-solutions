import Data.Char
import Data.List.Split

my_hash :: String -> Int -> Int
my_hash [] acc = acc
my_hash (x:xs) acc = my_hash xs new_acc
        where new_acc = 17*(ord x + acc) `mod` 256



main = do
        raw_c <- getContents
        let c = filter (/='\n') raw_c
        let hashes = map (flip my_hash 0) $ splitOn "," c
        print $ sum hashes
