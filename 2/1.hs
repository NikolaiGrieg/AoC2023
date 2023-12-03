module Main where

import Data.List.Split
import Data.List (nub)


data Bag = Bag{
    _id :: Int,
    blue :: Int,
    red :: Int,
    green :: Int
} deriving Show

limit = Bag 0 14 12 13

main = do  
    contents <- readFile "large.txt"
    let l = lines  contents
    --print $ toBag 321 ["3","blue"]
    --print "--------------"
    --print $ splitAndParseWithId $ (123, [["3 blue","4 red"], ["1 red","2 green","6 blue"], ["2 green"]])
    --print "-------------"
    let bags = concat $ map parseFullWithId l
    let invalidIds = map _id  $ filter (not . isPossible) bags
    --print invalidIds
    let validIds = filter (not . ( `elem` invalidIds)) $ map _id  bags 
    --print $ validIds
    print "--------ANS----------"
    print $ sum $ nub validIds
    --print "-------------"
    --print l


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
splitAndParseWithId (id, ss) = map (toBag id) entries
                               where entries = map words $ concat ss

toBag :: Int -> [String] -> Bag
toBag id s  
            | color == "blue" = Bag id num 0 0
            | color == "red" = Bag id 0 num 0
            | color == "green" = Bag id 0 0 num
            | otherwise = Bag id 0 0 0
            where (countStr, color) = toTuple s 
                  num = read countStr



isPossible :: Bag -> Bool
isPossible b = blue b <= blue limit && red b <= red limit && green b <= green limit

--mergeBags :: [Bag] -> Bag
--mergeBags bs = foldr (\a b -> Bag (mergeId (_id a) (_id b)) (blue a + blue b) (red a + red b) (green a + green b) ) (Bag 0 0 0 0) bs
--
--mergeId :: Int -> Int -> Int
--mergeId a b
--          | a == b = a
--          | a == 0 = b
--          | b == 0 = a
--          | otherwise = a+b --todo is is a good idea?


toTuple :: [String] -> (String, String)
toTuple ss = (head ss, head $ tail ss) 


-- 3 blue, 4 red; -> [["3","blue"],["4","red"]]
readEntry :: String -> [[String]] 
readEntry s = map words (splitOn "," s)

