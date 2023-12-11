import Data.List
import Data.List.Split

main = interact $ show . sum . take 3 . reverse . sort . map (sum . map read) . splitWhen (=="") . lines
