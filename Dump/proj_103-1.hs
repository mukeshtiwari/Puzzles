import Data.List
import qualified Data.Set as DS

subset :: [a] -> [[a]]
subset xs = init.tail $ concat [combination xs k | k <- [0..n]] where n = length xs



combination :: [a] -> Int -> [[a]]
combination _   0    = [[]]
combination []  _    = []
combination (x:xs) n = ( map (x:)  (combination xs (n-1)) ) ++ combination xs n


checkOptimum ::(Num a , Ord a ) => [a] -> Bool
checkOptimum xs = optimumHelper ( subset xs )   where
        optimumHelper (x:xs)  = 



if checkValid x ( last xs ) then True else optimumHelper ( init xs ) where 
	checkValid x y = and [ sum_a /= sum_b , if len_a < len_b then sum_a < sum_b else if len_a > len_b then sum_a > sum_b else True ] where
		sum_a = sum x 
		sum_b = sum y
		len_a = length x
		len_b = length y
	 
				


main = do 
	let lst_1 = filter checkOptimum $ combination [6..14] 5
            ans = foldl' (\(acc , list )  x -> if sum x < acc then ( sum x , x )  else ( acc , list) ) ( 1000 , [] ) lst_1		
	print ans 
