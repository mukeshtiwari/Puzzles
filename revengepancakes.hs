import Data.List

negatestr :: String -> String
negatestr t@('+' : _) = map (\_ -> '-') t
negatestr t@('-' : _) = map (\_ -> '+') t 

groupcakes :: String -> Int -> Int
groupcakes s cnt 
  | all (== '+') s = cnt
  | all (== '-') s = succ cnt
  | otherwise = groupcakes ret (cnt + 1) where
      (x : xs) = group s
      ret = concat ((negatestr x) : xs)

solve :: String -> Int
solve s = groupcakes s 0

main :: IO ()
main = interact $ unlines . zipWith (\x y -> "Case #" ++ show x ++ ": " ++ y) [1..] . map (show . solve) . tail . lines 
