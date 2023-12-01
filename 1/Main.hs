import Data.Char


main = do  
    contents <- readFile "small.txt"
    let l = lines  contents
    let nums = map parseFirstNum l
    print nums

parseFirstNum :: String -> Int
parseFirstNum (x:xs) 
    | isDigit x = digitToInt x
    | otherwise = parseFirstNum xs

--parseNums :: String -> Int
--parseNums s = read s


--isNumber :: String -> Bool
--isNumber str =
--    case (reads str) :: [(Double, String)] of
--      [(_, "")] -> True
--      _         -> False
