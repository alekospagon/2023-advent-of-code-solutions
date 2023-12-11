import Data.List.Split

main = interact $ show . maximum . map (sum . map read) . splitWhen (=="") . lines
