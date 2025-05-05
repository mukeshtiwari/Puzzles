import Data.List
import Data.Bits
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

bSearch :: ( Num a , Integral a , Bits a ) => [ a ] -> a -> a -> a
bSearch [ a , b , c , d ] low high = helpBsearch low high where 
	helpBsearch lo hi 
          | lo >= hi = lo  
	  | fuN lo * fuN mid > 0 = helpBsearch ( succ mid ) hi
	  | otherwise = helpBsearch lo mid
	  where mid = shiftR ( lo + hi )  1
		fuN x = ( ( a * x + b ) * x + c ) * x + d  



solve :: ( Num a , Integral  a , Bits a ) => [ a ] ->  BS.ByteString
solve [n , phi , sigma ] = roots    where 
	( a , b , c , d )  = ( 2 , 2 * n - phi - sigma ,  sigma - phi - 2 , -2 * n )
	dis =   sqrt . fromIntegral $ 4 * b ^ 2 - 12 * a * c 
	low =ceiling $ ( ( fromIntegral $  -2 * b  )  - dis  ) / ( fromIntegral $  6 * a  )
	high = ceiling $  ( ( fromIntegral $  -2 * b  )   +  dis  ) / ( fromIntegral $  6 * a  )
	r = bSearch [ a , b , c , d ]  low  high 
	d1 = -( b + r * a ) 
	d2 = truncate.sqrt.fromIntegral $ b ^ 2 - 4 * a * c - 2 * a * b * r - 3 * a ^ 2 * r ^ 2 
	r1 = div ( d1 - d2 ) ( 2 * a ) 
	r2 = div ( d1 + d2 ) ( 2 * a )
	roots =  BS.append  ( BS.append ( BS.append ( BS.append ( BS.pack.show $ r1 ) ( BS.pack " " ) ) ( BS.pack.show $ r ) ) ( BS.pack " " ) ) ( BS.pack.show $ r2 ) 


readInt :: BS.ByteString -> Integer
readInt = fst . fromJust . BS.readInteger  


main = BS.interact $ BS.unlines . map ( solve . map readInt . BS.words ) . tail . BS.lines  
