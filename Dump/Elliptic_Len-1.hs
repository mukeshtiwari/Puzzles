--y^2= x^3+ax+b mod n
import Random
import Control.Monad
import Data.List
import Data.Bits

data Elliptic = Conelliptic Integer Integer Integer deriving ( Eq , Show )
data Point = Conpoint Integer Integer | Identity deriving ( Eq , Show )



powM :: Integer -> Integer -> Integer -> Integer
powM a d n
 | d == 0 = 1
 | d == 1 = mod a n
 | otherwise = mod q n  where
	   p = powM  ( mod ( a^2 ) n ) ( shiftR d 1 ) n
	   q = if (.&.) d 1 == 1 then mod ( a * p ) n else p


calSd :: Integer -> IO ( Integer , Integer ) 
calSd n = return ( s , d ) where 
		s = until ( \x -> testBit ( n - 1) ( fromIntegral x ) )  ( +1 ) 0
                d = div ( n - 1 ) (  shiftL 1 ( fromIntegral s )  )


isProbable::Integer->IO Bool
isProbable n 
   | n <= 1 = return False
   | n == 2 = return True
   | even n = return False
   | otherwise	= calSd n >>= (\( s , d ) -> rabinMiller 0 n s d )


rabinMiller::Integer->Integer->Integer->Integer->IO Bool
rabinMiller cnt n s d 
   | cnt>=5= return True
   | otherwise = randomRIO ( 2 , n - 2 ) >>= 
                     (\a -> case powM a d n == 1 of 
				  True  -> rabinMiller ( cnt + 1 ) n s d
				  _   -> if any ( == pred n ) . take ( fromIntegral s ) . iterate (\e -> mod ( e^2 ) n ) $ powM a d n  then rabinMiller ( cnt + 1) n s d 
					 else return False )

{--
--add points of elliptic curve
addPoints :: Elliptic -> Point -> Point -> Either Point Integer
addPoints _ Identity p_2 = Left p_2
addPoints _ p_1 Identity = Left p_1
addPoints ( Conelliptic a b n ) ( Conpoint x_p y_p ) ( Conpoint x_q y_q )  
        | x_p /= x_q =  case ( ( Conpoint x_r y_r ) , d ) of
                          	( _ , 1 ) -> Left ( Conpoint x_r y_r )
                          	( _ , d' ) -> Right d'
                        	 where
                                	[ u , v , d ] = extended_gcd ( x_p - x_q ) n
                                	s = mod ( ( y_p - y_q ) * u ) n
                                	x_r = mod ( s*s - x_p - x_q ) n
                                	y_r = mod ( -y_p - s * ( x_r - x_p ) ) n

        | otherwise =   if mod ( y_p + y_q ) n == 0 then Left Identity
                         else case ( ( Conpoint x_r y_r ) , d ) of
                                    ( _ , 1 ) -> Left ( Conpoint x_r y_r )
                                    ( _ , d' ) -> Right d'
                                  where
                                	[ u , v , d ] = extended_gcd ( 2 * y_p ) n
                                	s = mod ( ( 3 * x_p * x_p + a ) * u ) n
                                	x_r = mod ( s * s - 2 * x_p ) n
                                	y_r = mod ( -y_p - s * ( x_r - x_p ) ) n


--}


--add points of elliptic curve
addPoints::Elliptic->Point->Point-> Either Point Integer
addPoints _ Identity p_2 = Left p_2
addPoints _ p_1 Identity = Left p_1
addPoints ( Conelliptic a b n ) ( Conpoint x_p y_p ) ( Conpoint x_q y_q )
	| x_p /= x_q = let
			   [ u , v , d ] = extended_gcd (x_p-x_q) n
			   s = mod  ( ( y_p - y_q ) * u ) n
			   x_r = mod ( s * s - x_p - x_q ) n
			   y_r= mod ( -y_p - s * ( x_r - x_p ) ) n
		     in case ( ( Conpoint x_r y_r ) , d ) of
			  ( _ , 1 ) -> Left ( Conpoint x_r y_r )
			  ( _ , d' ) -> Right d'
	| otherwise = if mod ( y_p + y_q ) n == 0 then Left Identity
		     else  let
			      [ u , v , d ] = extended_gcd ( 2*y_p ) n
			      s = mod  ( ( 3 * x_p * x_p + a ) * u ) n
			      x_r = mod ( s * s - 2 * x_p ) n
			      y_r = mod ( -y_p - s * ( x_r - x_p ) ) n
			   in case ( ( Conpoint x_r y_r ) , d ) of
                                ( _ , 1 )-> Left (Conpoint x_r y_r)
                                ( _ , d' ) -> Right d'



extended_gcd::Integer->Integer->[Integer]
extended_gcd u v= helpGcd u v 0 [ [ 1 , 0 ] , [ 0 , 1 ] ] where
	helpGcd u v n m @ ( [ [ a , b ] , [ c , d ] ] ) 
	  | v == 0 = if u<0 then [ - ( ( -1 ) ^ n ) * ( m !! 1 !! 1 ) , - ( ( -1 ) ^ ( n + 1 ) ) * ( m !! 0 !! 1 ) , -u ]
           	      else [ ( ( -1 ) ^ n ) * ( m !! 1 !! 1 ) , ( ( -1 ) ^ ( n + 1 ) ) * ( m !! 0 !! 1 ) , u ]
	  | otherwise = helpGcd u' v' ( n + 1 ) m' where 
		( q , v' ) = quotRem u v 
		t = [ [q , 1 ] , [ 1 , 0 ] ]
		m' = [ [ q * a + b , a ] , [ q * c + d , c ] ] --mult m t 
		u' = v 
  



multiEC :: Elliptic -> Point -> Integer -> IO ( Either Point Integer )
multiEC _ _ 0 = return $ Left Identity
multiEC ecurve point k | k>0 = return $ helpEC Identity point k
	where
	  helpEC p _ 0 = Left p
	  helpEC p q n = 
             case (p',q') of
		  ( Left p'' , Left q'' ) -> helpEC p'' q'' (div n 2)
		  ( Right p'' , _ ) -> Right p''
		  ( _ , Right q'' ) -> Right q''
	          where
		   p' =if odd n then addPoints ecurve p q else Left p
	           q' =  addPoints ecurve q q
			    

dscrmntEC a b = 4 * a * a * a + 27 * b * b


randomCurve :: Integer -> IO [Integer]
randomCurve n = randomRIO ( 1 , n ) >>= ( \a -> randomRIO ( 1 , n ) >>= ( \u -> randomRIO (1 , n) >>= (\v -> return [a , mod ( v*v - u*u*u - a*u ) n , n , u , v ] ) ) ) 


factor :: Integer -> Integer -> IO [Integer]
factor 1 _   = return []
factor n m   = 
    isProbable n >>= (\x -> if x then  return [n] 
			     else 
			       randomCurve n >>= (\[ a , b , n , u , v ] -> 
                             	 multiEC ( Conelliptic a b n ) ( Conpoint u v ) m >>= 
                                   ( \p -> case p of 
                                              Left p' -> factor n m 
                                              Right p'->  factor ( div n p' ) m  >>= ( \x -> factor p' m  >>= (\y  -> return $ x ++ y   ) ) ) ) )




solve :: Integer -> IO [ Integer ]
solve n = factor n ( foldl lcm 1 [1..10000] )   

main = liftM read getLine >>=( \n -> solve n  ) >>= print
