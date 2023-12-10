import Data.List.Split

myzip :: [String] -> [(Int, String)]
myzip [] = []
myzip (a:b:rest) = (read a, b):(myzip rest)


handfuls_acc :: (Int,Int,Int) -> [(Int,String)] -> (Int,Int,Int)
handfuls_acc (r,g,b) [] = (r,g,b)
handfuls_acc (r,g,b) ((num, colour):rest) = handfuls_acc (acc colour) rest
        where
        acc "red"   = (r+num, g, b)
        acc "green" = (r, g+num, b)
        acc "blue"  = (r, g, b+num)


my_min :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
my_min (r1,g1,b1) (r2,g2,b2) = (max r1 r2, max g1 g2, max b1 b2)


power :: String -> Int
power game = r*g*b
        where
        metadata:text = splitOn ":" game -- seperate first part
        game_id  = read $ drop 5 metadata :: Int -- read game_ID
        raw_handfuls = splitOn ";" (drop 1 $ filter (/=',') $ head text) -- split
        handfuls = map (myzip . words) raw_handfuls -- make tuples
        tuples = map (handfuls_acc (0,0,0)) handfuls
        (r,g,b) = foldr (my_min) (head tuples) tuples -- check validity


main = do
        input <- getContents
        print $ sum $ map power $ lines input
