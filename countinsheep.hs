import qualified Data.Set as S

convertList :: Integral a => a -> [a]
convertList 0 = [0]
convertList n = innerConvertList n where
   innerConvertList :: Integral a => a  -> [a]
   innerConvertList 0 = []
   innerConvertList v = mod v 10 : innerConvertList (div v 10)


iterateNumber :: Integral a => a -> a -> S.Set a -> a
iterateNumber n cnt set = 
    if length (S.toList set) == 10 then (n * cnt)
    else iterateNumber n (cnt + 1) (S.union set . S.fromList . convertList $ (cnt * n))


solve :: Integer -> String
solve 0 = "INSOMNIA"
solve n = show (iterateNumber n 1 S.empty - n)

main :: IO ()
main = interact $ unlines . zipWith (\x y -> "Case #" ++ show x ++ ": " ++ y) 
                  [1..] . map (solve . read) . tail . lines
