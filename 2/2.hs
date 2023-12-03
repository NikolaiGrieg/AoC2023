module Main where

import Data.List.Split
import Data.List (groupBy)
import Data.List (nub)


data Bag = Bag{
    _id :: Int,
    segment:: Int,
    blue :: Int,
    red :: Int,
    green :: Int
} deriving Show

limit = Bag 0 0 14 12 13

main = do  
    contents <- readFile "large.txt"
    let l = lines  contents
    let bags = concat $ map parseFullWithId l
    let maxedBags =  map maxBag $ groupBy (\a b -> (_id a) == (_id b)) bags
    print "--------ANS----------"
    print $ sum $ map (\x -> sum[(blue x)* (green x)* (red x)]) maxedBags


maxBag :: [Bag] -> Bag
maxBag bs = foldr (\a b -> Bag (_id a) (segment a) (max (blue a) (blue b)) (max (red a) (red b)) (max (green a) (green b)) ) (Bag 0 0 0 0 0) bs


parseFullWithId :: String -> [Bag]
parseFullWithId s = splitAndParseWithId (id, countEntriesSplit)
                    where splits = splitOn ":" s
                          gameSplit = head splits
                          countSplit = head $ tail splits
                          countEntries = splitOn ";" countSplit
                          countEntriesSplit = map (splitOn ",") countEntries
                          id = read $ head $ tail $ words gameSplit

-- (id, [["3 blue","4 red"], ["1 red","2 green","6 blue"], ["2 green"]]))
splitAndParseWithId :: (Int, [[String]]) -> [Bag]
splitAndParseWithId (id, ss) = map (toBag id  0) entries
                               where entries = map words $ concat ss

toBag :: Int -> Int -> [String] -> Bag
toBag id seg s  
            | color == "blue" = Bag id seg num 0 0
            | color == "red" = Bag id seg 0 num 0
            | color == "green" = Bag id seg 0 0 num
            | otherwise = Bag id seg 0 0 0
            where (countStr, color) = toTuple s 
                  num = read countStr


isPossible :: Bag -> Bool
isPossible b = blue b <= blue limit && red b <= red limit && green b <= green limit


toTuple :: [String] -> (String, String)
toTuple ss = (head ss, head $ tail ss) 

-- 3 blue, 4 red; -> [["3","blue"],["4","red"]]
readEntry :: String -> [[String]] 
readEntry s = map words (splitOn "," s)

