import Data.List
import Control.Monad

solve :: [Integer] -> String 
solve xs = solveHelp xs [] 1 where
	solveHelp [] ys _ = if ys == sort ys then "yes" else "no"
	solveHelp ( x:xs' ) ys i = case i == x of 
	    True -> solveHelp xs' ys (i+1) 
	    _    -> case ys of 
		      (y:ys') -> if i == y then solveHelp xs' ys' ( i + 1 ) else solveHelp xs' (x:y:ys') i
		      _       -> solveHelp xs' (x:ys) i
		
check :: [String] -> [String]
check [] = []
check (_:x:xs) = x:check xs


main = interact $ unlines . map ( solve . map read . words ) . check . init . lines
