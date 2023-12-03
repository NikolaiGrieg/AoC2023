module Main where

import Data.List.Split

data Bag = Bag{
    _id :: Int,
    blue :: Int,
    red :: Int,
    green :: Int
} deriving Show

--parse = (entryToBag . readEntry)
--splitAndParse = (\x ->  map parse $ splitOn ";" x)
--parseFull = (\x -> splitAndParse $ head $ tail $ splitOn ":" x) -- remove game id prefix
processLine = mergeBags . parseFullWithId

limit = Bag 0 14 12 13

main = do  
    contents <- readFile "small.txt"
    let l = lines  contents
    print $ toBag 321 ["3","blue"]
    print "--------------"
    print $ splitAndParseWithId $ (123, [["3 blue","4 red"], ["1 red","2 green","6 blue"], ["2 green"]])
    --print "----------------"
    --print $ mergeBags $ concat $ parseFull "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    --mapM_ print $  map parseFullWithId l
    print "-------------"
    print $ filter isPossible $ map processLine l
    print "--------ANS----------"
    print $ mergeBags $ filter isPossible $ map processLine l
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

mergeBags :: [Bag] -> Bag
mergeBags bs = foldr (\a b -> Bag (mergeId (_id a) (_id b)) (blue a + blue b) (red a + red b) (green a + green b) ) (Bag 0 0 0 0) bs

mergeId :: Int -> Int -> Int
mergeId a b
          | a == b = a
          | a == 0 = b
          | b == 0 = a
          | otherwise = a+b --todo is is a good idea?

entryToBag :: [[String]] -> [Bag]
entryToBag ss = map (bag . toTuple) ss

toTuple :: [String] -> (String, String)
toTuple ss = (head ss, head $ tail ss) 


bag :: (String, String) -> Bag
bag (numStr, color)  
                | color == "blue" = Bag 0 num 0 0
                | color == "red" = Bag 0 0 num 0
                | color == "green" = Bag 0 0 0 num
                | otherwise = Bag 0 0 0 0
                  where num = read numStr

-- 3 blue, 4 red; -> [["3","blue"],["4","red"]]
readEntry :: String -> [[String]] 
readEntry s = map words (splitOn "," s)

readId :: String -> String
readId s = head (splitOn ":" s)
