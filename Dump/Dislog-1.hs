import Data.List
import qualified Data.Sequence as DS
import Data.Bits
--g^l = t mod p 


extendedGcd :: ( Num a , Integral a ) => a -> a -> ( a , a )
extendedGcd a b
   | b == 0 = ( 1 , 0 )
   | otherwise = ( t , s - q * t ) where
	( q , r ) = quotRem a b
	( s , t ) = extendedGcd b r 


modInv :: ( Num a , Integral a ) => a -> a -> a
modInv  a b =  d where
	d = until ( > 0 ) ( + b  ) . fst.extendedGcd a  $ b 
 

powM :: ( Num a , Integral a , Bits a ) => a -> a -> a -> a
powM a d n =  powM' a d n where
  powM' a d n
        | d == 0 = 1
        | d == 1 = mod a n
        | otherwise = mod q n  where
	       p = powM'  ( mod ( a^2 ) n ) ( shiftR d 1 ) n
	       q = if (.&.) d 1 == 1 then mod ( a * p ) n else p
 


helpfN :: ( Num a , Integral a , Bits a ) => a -> a -> a -> a -> a -> a -> ( a , a , a ) 
helpfN x a b g t p 
 | and [ 0 < x , 3 * x < p ] = ( mod ( t * x ) p , mod ( a + 1 ) ( p - 1 ) , b ) 
 | and [ p < 3 * x , 3 * x <  2 * p ] = ( mod ( x^2 ) p , mod ( 2 * a ) ( p - 1 ) , mod ( 2 * b ) ( p - 1 ) ) 
 | and [ 2 * p < 3 * x , x < p ] = ( mod ( g * x ) p , a , mod ( b + 1) ( p - 1 ) )
 | otherwise = error " some thing wrong " 


solve :: ( Num a , Integral a , Bits a ) => a -> a -> a -> a -> a -> a ->  a -> a -> a -> a  
solve x1 a1 b1 x2 a2 b2 g t p
 | x1 == x2 = l 
 | otherwise = solve x1' a1' b1' x2' a2' b2' g t p where 
	( x1' , a1' , b1' ) = helpfN x1 a1 b1 g t p
	( xt , at , bt ) = helpfN x2 a2 b2 g t p
	( x2' , a2' , b2' ) = helpfN xt at bt g t p 
	d = gcd ( a2 - a1 ) ( p - 1 ) 
	l0 = ( b1 - b2 ) *  modInv (  a2 - a1 ) ( div ( p - 1 ) d )
	l = head $ filter ( \x -> powM g ( l0 + div ( x * ( p - 1 ) ) d  ) p == t  )  [ 0 .. pred d ]             --until (  >= 0 ) ( + ( div ( p - 1 ) d  ) ) l0  



final :: ( Num a , Integral a , Bits a ) => a -> a -> a -> a 
final g t p = solve x1 a1 b1 x2 a2 b2 g t p where 
	( x1 , a1 , b1 ) = helpfN 1 0 0 g t p
	( x2 , a2 , b2 ) = helpfN x1 a1 b1 g t p  

