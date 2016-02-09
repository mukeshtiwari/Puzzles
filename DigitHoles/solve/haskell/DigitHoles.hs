module DigitHoles where

numHoles :: Int -> Int
numHoles 0 = 0
numHoles number = final . numHoles' $ number where
    numHoles' :: Int -> [Int]
    numHoles' 0 = []
    numHoles' num = mod num 10 : numHoles' (div num 10)
    final :: [Int] -> Int
    final xs = foldr step 0 xs
    step :: Int -> Int -> Int
    step x acc
     | x == 0 || x == 4 || x == 6 || x == 9 = acc + 1
     | x == 8 = acc + 2
     | otherwise = acc
