import Data.List

input :: IO [String]
input = do 
    c <- readFile "input.txt" 
    return (lines c)

hasNRepeating :: Int -> String -> Bool
hasNRepeating n s = elem n $ map length $ group $ sort s

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

ex1 :: IO Int
ex1 = do
    i <- input
    let has2 = sum $ map (boolToInt . hasNRepeating 2) i
    let has3 = sum $ map (boolToInt . hasNRepeating 3) i
    return $ has2 * has3

isOneApart :: Eq a => [a] -> [a] -> Bool
isOneApart a b = (sum $ map boolToInt $ zipWith (==) a b) == (length a - 1)

onesApart :: Eq a => [[a]] -> [[a]] -> [([a], [a])]
onesApart a b = [(x, y) | x <- a, y <- b, isOneApart x y]

inCommon :: Eq a => [a] -> [a] -> [a]
inCommon [] _ = []
inCommon _ [] = []
inCommon (x:xs) (y:ys) | x == y    = x : inCommon xs ys
                       | otherwise = inCommon xs ys

ex2 = do
    i <- input
    return $ uncurry inCommon $ head $ onesApart i i
    