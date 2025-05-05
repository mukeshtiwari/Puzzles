import Data.List
import Data.Char

main = print $ length . filter (\x-> all odd . map digitToInt . show $ x ). map (\x -> x + ( read.reverse.show )  x) . filter (\x-> mod  x  10 /= 0 ) $ [1..10^3-1]
