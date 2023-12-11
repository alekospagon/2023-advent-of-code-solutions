import Data.List.Split

myzip :: [String] -> [(Int, String)]
myzip [] = []
myzip (a:b:rest) = (read a, b):(myzip rest)


handfuls_validity :: (Int,Int,Int) -> [(Int,String)] -> Bool
handfuls_validity (r,g,b) [] = r<=12 && g<=13 && b <= 14
handfuls_validity (r,g,b) ((num, colour):rest) = handfuls_validity (acc colour) rest
        where
        acc "red"   = (r+num, g, b)
        acc "green" = (r, g+num, b)
        acc "blue"  = (r, g, b+num)


solve game = if validity then game_id else 0
        where
        metadata:text = splitOn ":" game -- seperate first part
        game_id  = read $ drop 5 metadata :: Int -- read game_ID
        raw_handfuls = splitOn ";" (drop 1 $ filter (/=',') $ head text) -- split
        handfuls = map (myzip . words) raw_handfuls -- make tuples
        validity = all (handfuls_validity (0,0,0)) handfuls -- check validity


main = do
        input <- getContents
        print $ sum $ map solve $ lines input
