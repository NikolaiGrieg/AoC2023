module Main where

import Data.List.Split

data Bag = Bag{
    id :: String,
    blue :: Int,
    red :: Int,
    green :: Int
} deriving Show

parse = (entryToBag . readEntry)
splitAndParse = (\x ->  map parse $ splitOn ";" x)
parseFull = (\x -> splitAndParse $ head $ tail $ splitOn ":" x) -- remove game id prefix
processLine = mergeBags . concat . parseFull

limit = Bag "" 14 12 13

main = do  
    contents <- readFile "small.txt"
    let l = lines  contents
    print $ parseFull "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    print "--------------"
    -- TODO parse IDs and add them up
    --print $ mergeBags $ concat $ parseFull "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    mapM_ print $  map processLine l
    print "-------------"
    print $ map isPossible $ map processLine l
    print "-------------"
    print l


isPossible :: Bag -> Bool
isPossible b = blue b <= blue limit && red b <= red limit && green b <= green limit

mergeBags :: [Bag] -> Bag
mergeBags bs = foldr (\a b -> Bag "" (blue a + blue b) (red a + red b) (green a + green b) ) (Bag "" 0 0 0) bs

entryToBag :: [[String]] -> [Bag]
entryToBag ss = map (bag . toTuple) ss

toTuple :: [String] -> (String, String)
toTuple ss = (head ss, head $ tail ss) 


bag :: (String, String) -> Bag
bag (numStr, color)  
                | color == "blue" = Bag "" num 0 0
                | color == "red" = Bag "" 0 num 0
                | color == "green" = Bag "" 0 0 num
                | otherwise = Bag "" 0 0 0
                  where num = read numStr

-- 3 blue, 4 red; -> [["3","blue"],["4","red"]]
readEntry :: String -> [[String]] 
readEntry s = map words (splitOn "," s)

readId :: String -> String
readId s = head (splitOn ":" s)
