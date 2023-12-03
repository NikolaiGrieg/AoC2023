module Main where

import Data.List.Split
import Data.Char (isDigit)

--data Part = Part{
--    num :: Int,
--    x1 :: Int,
--    x2 :: Int,
--    y1 :: Int,
--    y2 :: Int
--} deriving Show

data Symbol = Symbol{
    symbol :: Char,
    _x1 :: Int,
    _x2 :: Int,
    _y1 :: Int,
    _y2 :: Int
}

data Point = Point{
    c :: Char,
    x :: Int,
    y :: Int
} deriving Show


main = do  
    contents <- readFile "small.txt"
    let l = lines  contents
    let firstl = head l
    
    let points = concat $ map toPoints $ zip [0..] l
    mapM_ print points
    print "---------------"
    print $ map parseParts l

toPoints :: (Int, String) -> [Point]
toPoints (y, s) = map (toPoint y) $ zip [0..] s

toPoint :: Int -> (Int, Char) -> Point
toPoint y (x, c) = Point c x y

-- todo how to parse this with coordinate info??
-- maybe parse each digit to coords (like Symbol) and merge them?
-- mergeSymbols $ groupBy (\a b -> isConsequtive a b) symbols

parseParts :: String -> [String]
parseParts s = splits
           where splits = splitWhen (not . isDigit) s 
                 validSplits = filter (\x -> x /= "") splits
