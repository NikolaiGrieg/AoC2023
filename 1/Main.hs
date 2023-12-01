import Data.Char
import Data.List

main = do  
    contents <- readFile "small.txt"
    let l = lines  contents
    let nums =  map  parseAllNums l
    let ans = map stringConcatNum $ map firstAndLast nums
    print nums
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

--parseNums :: String -> Int
--parseNums s = read s


--isNumber :: String -> Bool
--isNumber str =
--    case (reads str) :: [(Double, String)] of
--      [(_, "")] -> True
--      _         -> False
