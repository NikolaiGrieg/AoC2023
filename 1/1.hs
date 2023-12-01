import Data.Char
import Data.List

main = do  
    contents <- readFile "large.txt"
    let l = lines  contents
    let nums =  map  parseAllNums l
    let ans = sum $ map stringConcatNum $ map firstAndLast nums
    print ans

firstAndLast :: [a] -> [a]
firstAndLast xs = head xs:[(last xs)]

stringConcatNum :: [Int] -> Int
stringConcatNum xs = read $ strConcat (map show xs)

strConcat :: [String] -> String
strConcat xs = foldr (\a b -> a ++ b) "" xs

parseFirstNum :: String -> Int
parseFirstNum (x:xs) 
    | isDigit x = digitToInt x
    | otherwise = parseFirstNum xs


parseAllNums :: String -> [Int]
parseAllNums s = [digitToInt x | x <- s, isDigit x]
