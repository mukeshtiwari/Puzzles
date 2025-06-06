import Data.List
import Data.Array
import Data.Maybe

data Point a = P a a deriving ( Show , Ord , Eq ) 
data Vector a = V a a deriving ( Show , Ord , Eq ) 
data Turn = S | L | R deriving ( Show , Eq , Ord , Enum  )


--start of convex hull

compPoint :: ( Num  a , Ord a ) => Point a -> Point a -> Ordering
compPoint ( P x1 y1 ) ( P x2 y2 )
  | compare x1 x2 == EQ = compare y1 y2
  | otherwise = compare x1 x2 

sortPoint :: ( Num a , Ord a ) => [ Point a ] -> [ Point a ]
sortPoint xs = sortBy ( \ x y -> compPoint x y ) xs

findTurn :: ( Num a , Ord a , Eq a ) => Point a -> Point a -> Point a -> Turn
findTurn ( P x0 y0 ) ( P x1 y1 ) ( P x2 y2 )
 | ( y1 - y0 ) * ( x2- x0 ) < ( y2 - y0 ) * ( x1 - x0 ) = L
 | ( y1 - y0 ) * ( x2- x0 ) == ( y2 - y0 ) * ( x1 - x0 ) = S
 | otherwise = R 

hullComputation :: ( Num a , Ord a ) => [ Point a ] -> [ Point a ] -> [ Point a ]
hullComputation [x] ( z:ys ) = hullComputation [z,x] ys
hullComputation xs [] = xs
hullComputation  ( y : x : xs ) ( z : ys )
  |  findTurn x y z == R = hullComputation ( x:xs ) ( z : ys )
  |  findTurn x y z == S = hullComputation ( x:xs ) ( z : ys )
  |  otherwise = hullComputation ( z : y : x : xs ) ys 

convexHull :: ( Num a , Ord a ) => [ Point a ] -> [ Point a ]
convexHull [] = []
convexHull [ p ] =  [ p ]
convexHull [ p1 , p2 ] = [ p1 , p2 ]
convexHull xs = final where
	txs = sortPoint xs
	( x : y : ys  ) = txs
        lhull = hullComputation [y,x] ys
	( x': y' : xs' ) = reverse txs
	uhull = hullComputation [ y' , x' ] xs'
	final = ( init lhull ) ++ ( init uhull )  

--end of convex hull 


--dot product for getting angle
angVectors :: ( Num a , Ord a , Floating a ) => Vector a -> Vector a -> a 
angVectors ( V ax ay ) ( V bx by ) = theta where 
    dot = ax * bx + ay * by 
    a = sqrt $ ax ^ 2 + ay ^ 2 
    b = sqrt $ bx ^ 2 + by ^ 2 
    theta = acos $ dot / a / b  

--start of rotating caliper part http://en.wikipedia.org/wiki/Rotating_calipers

--rotate the vector x y by angle t 
rotVector :: ( Num a , Ord a , Floating a ) => Vector a -> a -> Vector a 
rotVector ( V x y ) t = V ( x * cos t - y * sin t ) ( x * sin t + y * cos t )  

--square of dist between two points 

distPoints :: ( Num a , Ord a , Floating a ) => Point a -> Point a -> a
distPoints ( P x1 y1 ) ( P x2 y2 ) =  ( x1 - x2 ) ^ 2 + ( y1 - y2 ) ^ 2 

--rotating caliipers 

rotCal :: ( Num a , Ord a , Floating a ) => [ Point a ] -> a -> Int -> Int -> Vector a -> Vector a -> a -> Int -> IO a 
rotCal arr ang  pa pb ca@( V ax ay ) cb@( V bx by ) dia n 
   | ang > pi = return dia 
   | otherwise = print ang >>  rotCal arr ang' pa' pb' ca' cb' dia' n where 
	P x1 y1 = arr !! pa
	P x2 y2 = arr !! ( mod ( pa + 1 ) n )
	P x3 y3 = arr !! pb 
	P x4 y4 = arr !! ( mod ( pb + 1 ) n ) 
	t1 = angVectors ca ( V ( x2 - x1 ) ( y2 - y1 ) )
	t2 = angVectors cb ( V ( x4 - x3 ) ( y4 - y3 ) )
	ca' = rotVector ca  $ min t1 t2 
	cb' = rotVector cb  $ min t1 t2
	ang' = ang + min t1 t2 
	pa' = if t1 < t2 then mod ( pa + 1 ) n else pa 
	pb' = if t1 >= t2 then mod ( pb + 1 ) n else pb
	dia' = max dia $ distPoints ( arr !! pa' ) ( arr !! pb' ) 
	--dia' = max dia  $ if t1 < t2 then distPoints ( arr !! pa' ) ( arr !! pb ) else distPoints ( arr !! pb' ) ( arr !! pa )


solve :: ( Num a , Ord a , Floating a ) => [ Point a ] -> IO String 
solve [] = return "0"
solve [ p ] = return "0"
solve [ p1 , p2 ] = return . show $ distPoints p1 p2
solve [ p1 , p2 , p3 ] = return . show $ max ( distPoints p1 p2 ) $ max ( distPoints p2 p3 ) ( distPoints p3 p1 ) 
solve arr =  rotCal arr' 0 pa pb ( V 1 0 ) ( V (-1) 0 ) dia n >>= (\x -> return.show $ x )  where 
	   arr' =  convexHull   arr 
	   y1 = minimumBy ( \( P _ y1 ) ( P _ y2 ) -> compare y1 y2 ) arr'
	   y2 = maximumBy ( \( P _ y1 ) ( P _ y2 ) -> compare y1 y2 ) arr'
	   pa = fromJust . findIndex ( \ t -> t == y1 ) $ arr' 
	   pb = fromJust . findIndex ( \ t -> t == y2 ) $ arr' 
	   dia = distPoints ( arr' !! pa ) ( arr' !! pb ) 
	   n = length arr'

 --end of rotating caliper 
{--
--spoj code for testing 
final :: ( Num a , Ord a , Floating a ) => [ Point a ] -> String
final [] = "0"
final [ p ] = "0"
final [ p1 , p2 ] = show $ distPoints p1 p2
final [ p1 , p2 , p3 ] = show $ max ( distPoints p1 p2 ) $ max ( distPoints p2 p3 ) ( distPoints p3 p1 )
final arr = solve . convexHull $ arr

format :: ( Num a , Ord a , Floating a ) => [ Int ] -> [ [ Point a ]]
format [] = []
format (x:xs ) =  t : format b  where 
	( a , b ) = splitAt ( 2 * x ) xs 
	t = helpFormat a where 
	    helpFormat [] = []	
	    helpFormat ( x' : y' : xs' ) = ( P ( fromIntegral x' ) ( fromIntegral  y' ) ) : helpFormat xs'

readD :: String -> Int
readD = read 


main = interact $ unlines . map  final . format . concat . ( map . map ) readD . map words . tail  . lines  

--}
--end of spoj code 
