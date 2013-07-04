import Data.List
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Lazy.Char8 as BS

bruteForce :: [ Int ] -> Int
bruteForce [] = 0
bruteForce [_] = 0
bruteForce ( x : xs ) = ( length . filter ( x > ) $ xs ) + bruteForce xs

{- compute the inversion count from two sorted list.-}
inversionCnt :: [ Int ] -> [ Int ] -> [ Int ] -> Int -> Int -> ( Int , [ Int ] )
inversionCnt [] ys  ret _ cnt = ( cnt , reverse ret ++ ys ) 
inversionCnt xs []  ret _ cnt = ( cnt , reverse ret ++ xs )
inversionCnt u@( x : xs ) v@( y : ys )  ret n cnt
      | x <= y = inversionCnt xs v  (  x : ret   )  ( pred n ) cnt
      | otherwise = inversionCnt u ys  ( y : ret ) n ( cnt + n )  

{-
inversionCnt :: [ Int ] -> [ Int ] -> [ Int ] -> Int -> ( Int , [ Int ] )
inversionCnt [] ys  ret  cnt = ( cnt , reverse ret ++ ys ) 
inversionCnt xs []  ret  cnt = ( cnt , reverse ret ++ xs )
inversionCnt u@( x : xs ) v@( y : ys )  ret  cnt
      | x <= y = inversionCnt xs v  (  x : ret   )   cnt
      | otherwise = inversionCnt u ys  ( y : ret )  ( cnt + length u )  
-}
merge ::  ( Int , [ Int ] ) -> ( Int , [ Int ] ) -> ( Int , [ Int ] )
merge ( cnt_1 , xs ) ( cnt_2 , ys ) = ( cnt_1 + cnt_2 + cnt , ret ) where
        ( cnt , ret ) = inversionCnt xs ys [] ( length xs )  0

mergeSort ::  [ Int ] -> ( Int , [ Int ] )
mergeSort [ x ] = ( 0 , [ x ] )
mergeSort  xs   = merge ( mergeSort xs' ) ( mergeSort ys' ) where
           ( xs' , ys' ) = splitAt ( div ( length xs ) 2 ) xs
          
main =  BS.interact $ ( \x -> BS.pack $ show x ++ "\n" ) . fst 
        . mergeSort . map ( fst . fromJust .  BS.readInt :: BS.ByteString -> Int  ) 
        . BS.lines 