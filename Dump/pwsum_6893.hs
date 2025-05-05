import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

--lucas theorem is not going to work here
base :: ( Integral a ) => a -> a -> [a] 
base 0 _ = [0] 
base n p = fst.until ( \( x , k ) -> k == 0 ) ( \( x , k ) -> ( mod k p : x , div k p ) ) $ ( [] , n )

{-- since m is hardly 300 so calculate the things in advance --}

binomM n k p 
   | 2*k >= n = mod ( div ( product [n , pred n .. succ k ] ) ( product [ 1 .. ( n - k ) ] ) )  p 
   | otherwise = mod ( div ( product [n , pred n .. ( n - k + 1 ) ] ) ( product [1 .. k ] ) ) p


extendedGcd :: Integer -> Integer -> ( Integer , Integer ) 
extendedGcd a b 
   | b == 0 = ( 1 , 0 ) 
   | otherwise = ( t , s - q * t ) where 
	( q , r ) = quotRem a b 
	( s , t ) = extendedGcd b r 
	 

modInv :: Integer -> Integer -> Integer 
modInv  a b 
   | gcd a b /= 1 = error " gcd is not 1 "
   | otherwise = d where 
	d = until ( > 0 ) ( + b  ) . fst.extendedGcd a $ b   

m ::  [Integer] 
m = map ( flip mod 10007 . flip  modInv 10007 ) [1..301]
