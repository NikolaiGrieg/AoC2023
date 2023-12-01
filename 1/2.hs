import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map



allDigits = Map.fromList( [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5),  ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)] ++ map (\x -> (show x, x)) [1..10])
main = do  
    contents <- readFile "large.txt"
    let l = lines  contents
    let nums =  map  specialParse l
    let ans = sum $ map stringConcatNum $ map firstAndLast nums
    print ans

specialParse :: String -> [Int]
specialParse xs = (findFirst xs):[findLast xs]


findFirst :: String  -> Int
findFirst []  = 0
findFirst (s:ss) 
          | hit = allDigits Map.! x
          | otherwise = findFirst ss 
            where (x, hit) = matchPrefix (s:ss) $ Map.keys allDigits

-- should have somehow injected the map since the entires are reversed relative to the passed keys here
findB :: String -> [String] -> Int
findB [] _ = 0
findB (s:ss) keys  
          | hit = allDigits Map.! (reverse x)
          | otherwise = findB ss keys 
            where (x, hit) = matchPrefix (s:ss) keys

findLast :: String -> Int
findLast [] = 0
findLast s = findB (reverse s) (map reverse $ Map.keys allDigits)

-- returns the matched prefix and true if hit
matchPrefix :: String -> [String] -> (String, Bool)
matchPrefix s [] = ("", False)
matchPrefix s (x:xs)
            | prefix x s = (x, True)
            | otherwise = matchPrefix s xs

suffix :: Eq a => [a] -> [a] -> Bool
suffix [] _ = True
suffix _ [] = False
suffix a b = prefix (reverse a) (reverse b)

-- from https://stackoverflow.com/questions/47733248/haskell-check-if-the-first-list-is-a-prefix-of-the-second
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys


firstAndLast :: [a] -> [a]
firstAndLast xs = head xs:[(last xs)]

stringConcatNum :: [Int] -> Int
stringConcatNum xs = read $ strConcat (map show xs)

strConcat :: [String] -> String
strConcat xs = foldr (\a b -> a ++ b) "" xs
