import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


allDigits = Map.fromList( [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5),  ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)] ++ map (\x -> (show x, x)) [1..10])
main = do  
    --print $ allDigits Map.! "4"
    --print $ matchPrefix  "two1" $ Map.keys allDigits
    contents <- readFile "small2.txt"
    let l = lines  contents
    let nums =  map  specialParse l
    --let ans = sum $ map stringConcatNum $ map firstAndLast nums
    print nums

specialParse :: String -> [Int]
specialParse xs = [findFirst xs]


findFirst :: String -> Int
findFirst [] = 0
findFirst (s:ss) 
          | hit = allDigits Map.! x
          | otherwise = findFirst ss 
            where (x, hit) = matchPrefix (s:ss) $ Map.keys allDigits

--findLast :: String -> Int
--todo


-- returns the matched prefix and true if hit
matchPrefix :: String -> [String] -> (String, Bool)
matchPrefix s [] = ("", False)
matchPrefix s (x:xs)
            | prefix x s = (x, True)
            | otherwise = matchPrefix s xs

-- from https://stackoverflow.com/questions/47733248/haskell-check-if-the-first-list-is-a-prefix-of-the-second
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys
--
--
--firstAndLast :: [a] -> [a]
--firstAndLast xs = head xs:[(last xs)]
--
--stringConcatNum :: [Int] -> Int
--stringConcatNum xs = read $ strConcat (map show xs)
--
--strConcat :: [String] -> String
--strConcat xs = foldr (\a b -> a ++ b) "" xs
--
--parseAllNums :: String -> [Int]
--parseAllNums s = [digitToInt x | x <- s, isDigit x]
