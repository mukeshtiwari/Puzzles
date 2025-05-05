import Data.List
import Data.Maybe

prime::[Integer]
prime = 2 : 3 : filter isPrime [ 2 * k + r | r <-[ -1 , 1 ] , k <-[3..] ]


isPrime :: Integer -> Bool 
isPrime n = all ( (/=0).mod n ).takeWhile (  <=  ( truncate.sqrt.fromIntegral $ n  ) ) $ prime 


priRoot :: Integer -> Integer
priRoot 2 = 1 
priRoot p = head $ filter ( \x -> helpRoot x p lst ) [2..] where 
	lst = foldl ( \x y -> if mod (p-1)  y  == 0 then x ++ [y] else x ) [] $ takeWhile (  <=  ( truncate.sqrt.fromIntegral $ p  ) )  prime 
	


helpRoot :: Integer -> Integer -> [ Integer ] -> Bool
helpRoot a n lst = all ( \x -> powM a ( div (n-1) x ) n /= 1 ) lst


powM :: Integer -> Integer -> Integer -> Integer
powM a 0 _ = 1
powM a 1 m = mod a m
powM a b m = if odd b then mod ( a * powM ( mod ( a^2 ) m ) ( div b 2 ) m ) m else mod ( powM ( mod ( a^2) m  ) ( div b 2 ) m ) m 


berModp :: Integer -> Integer -> Integer 
berModp k p = 
   let 
	g = priRoot p 
	r = powM g ( k - 1 ) p
	u = mod ( div ( g - 1 ) 2 ) p 
	s = 0 
	x = 1 
	y = r 
   in fun 1 ( div ( p - 1 ) 2 ) k g r u s x y p 

fun cnt lim k g r u s x y p 
   | cnt > lim = mod (  2*k*s * modInv ( mod ( 1 - powM g k p ) p )  p ) p 
   | otherwise = 
	let 
	   q' = div ( g * x ) p 
	   s' = mod ( s + ( u - q' ) * y ) p
	   x' = mod ( g*x ) p 
	   y' = mod ( r*y ) p
	in fun ( cnt + 1) lim k g r u s' x' y' p 


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

