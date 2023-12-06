import System.IO
import Data.Char (ord)
import GHC.List (head, tail)



scan_nums :: String -> [Int]
scan_nums [] = []
-- possible overlapping... be sure to prepend and valid prefixes
-- from the pattern's suffix
scan_nums ('o':'n':'e':xs) = 1 : (scan_nums ('e':xs))
scan_nums ('t':'w':'o':xs) = 2 : (scan_nums ('o':xs))
scan_nums ('t':'h':'r':'e':'e':xs) = 3 : (scan_nums ('e':xs))
scan_nums ('f':'o':'u':'r':xs) = 4 : (scan_nums ('o':xs))
scan_nums ('f':'i':'v':'e':xs) = 5 : (scan_nums ('e':xs))
scan_nums ('s':'i':'x':xs) = 6 : (scan_nums xs)
scan_nums ('s':'e':'v':'e':'n':xs) = 7 : (scan_nums ('n':xs))
scan_nums ('e':'i':'g':'h':'t':xs) = 8 : (scan_nums ('t':xs))
scan_nums ('n':'i':'n':'e':xs) = 9 : (scan_nums ('e':xs))
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
