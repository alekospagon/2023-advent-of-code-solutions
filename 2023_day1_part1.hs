import System.IO
import Data.Char (ord)
import GHC.List (head, tail)



scan_nums :: String -> [Int]
scan_nums [] = []
scan_nums (x:xs) = 
	if ord x >= 48 && ord x <= 57
	then (ord x - 48) : (scan_nums xs)
	else (scan_nums xs)



first_and_last :: [Int] -> Int
first_and_last [] = 0
first_and_last l = (head l) * 10 + (last l)


-- scan, then combine, then sum them
solutions strings = sum $ map (first_and_last . scan_nums) strings



main :: IO ()
main = interact $ show . solutions . lines
