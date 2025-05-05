import Data.List
import qualified Data.Sequence as DS 
import Text.Printf

data Point a = P a a deriving ( Show , Ord , Eq ) 
data Turn = S | L | R deriving ( Show , Eq , Ord , Enum  ) -- straight left right  


--start of convex hull  http://en.wikipedia.org/wiki/Graham_scan

compPoint :: ( Num  a , Ord a ) => Point a -> Point a -> Ordering
compPoint ( P x1 y1 ) ( P x2 y2 )
  | compare x1 x2 == EQ = compare y1 y2
  | otherwise = compare x1 x2 


findMinx :: ( Num a , Ord a ) => [ Point a ] -> [ Point a ]
findMinx xs = sortBy ( \x  y  -> compPoint  x y  ) xs


compAngle ::(Num a , Ord a ) => Point a -> Point a -> Point a -> Ordering
compAngle ( P x1 y1 ) ( P x2 y2 ) ( P x0 y0 ) = compare ( (  y1 - y0 ) * ( x2 - x0 )  ) ( ( y2 - y0) * ( x1 - x0 ) )


sortByangle :: ( Num a , Ord a ) => [ Point a ] -> [ Point a ]
sortByangle (z:xs) = z : sortBy ( \x y -> compAngle x y z ) xs 


convexHull ::( Num a , Ord a )	=> [ Point a ] -> [ Point a ]
convexHull [ P x0 y0 ] = [ P x0 y0 ]
convexHull xs = reverse . findHull [y,x]  $ ys where
	(x:y:ys) = sortByangle.findMinx $ xs 


findTurn :: ( Num a , Ord a , Eq a ) => Point a -> Point a -> Point a -> Turn
findTurn ( P x0 y0 ) ( P x1 y1 ) ( P x2 y2 )
 | ( y1 - y0 ) * ( x2- x0 ) < ( y2 - y0 ) * ( x1 - x0 ) = L
 | ( y1 - y0 ) * ( x2- x0 ) == ( y2 - y0 ) * ( x1 - x0 ) = S
 | otherwise = R 


findHull :: ( Num a , Ord a  )  => [ Point a ] ->   [ Point a ] -> [ Point a ]
findHull [x]  ( z : ys )  = findHull [ z , x ]  ys  --incase of second point  on line from x to z
findHull xs  [] = xs
findHull ( y : x : xs )  ( z:ys )
  | findTurn x y z == R = findHull (  x : xs )   ( z:ys )
  | findTurn x y z == S = findHull (  x : xs )   ( z:ys )
  | otherwise = findHull ( z : y : x : xs  )   ys



--end of convex hull 
--start of finding point algorithm http://www.personal.kent.edu/~rmuhamma/Compgeometry/MyCG/CG-Applets/Center/centercli.htm  Applet’s Algorithm 


findAngle :: ( Num a , Ord a , Floating a ) => Point a -> Point a -> Point  a  -> ( Point a , Point a , Point  a , a ) 
findAngle u@(P x0 y0 ) v@(P x1 y1 ) t@(P x2 y2)  = ( u , v, t , ang ) where
          	ang = acos ( ( b + c - a ) / ( 2 * sb * sc ) ) where 
			b = ( x0 - x2 ) ^ 2 + ( y0 - y2 ) ^ 2
			c = ( x1 - x2 ) ^ 2 + ( y1 - y2 ) ^ 2
			a = ( x0 - x1 ) ^ 2 + ( y0 - y1 ) ^ 2 
			sb = sqrt b
			sc = sqrt c 
	


findPoints :: ( Num a , Ord a , Floating a ) => Point a -> Point a -> [ Point  a ] -> ( Point a , Point a , Point a , a ) 
findPoints u v xs 
  | 2 * theta >= pi =  	( a , b , t , theta ) 
  | and [ 2 * alpha <= pi , 2 * beta <= pi ]   = ( a , b , t , theta )  
  | otherwise = if 2 * alpha > pi then findPoints v t xs else findPoints u t xs 
     where   
	( a , b , t , theta ) = minimumBy ( \(_,_,_, t1 ) ( _ , _ , _ ,t2 ) -> compare  t1 t2 ) . map ( findAngle u v )  $ xs 
        ( _ , _ , _ , alpha ) = findAngle v t u  --angle between v u t angle subtended at u by v t
	( _ , _ , _ , beta ) = findAngle u t v   -- angle between u v t angle subtended at v by  u t


--end of finding three points algorithm
--find the circle through three points http://paulbourke.net/geometry/circlefrom3/ 

circlePoints :: ( Num a , Ord a , Floating a ) => Point a -> Point a -> Point a -> ( Point a , a ) --( center , radius )
circlePoints u@(P x1 y1 ) v@(P x2 y2 ) t@(P x3 y3 ) 
	| x2 == x1 = circlePoints u t v 
	| x3 == x2 = circlePoints v u t 
	| otherwise =  ( P x y , 2 *  r )   
	  where 
		m1 = ( y2 - y1 ) / ( x2 - x1 ) 
		m2 = ( y3 - y2 ) / ( x3 - x2 ) 
		x = ( m1 * m2 * ( y1 - y3 ) + m2 * ( x1 + x2 ) - m1 * ( x2 + x3 ) ) / ( 2 * ( m2 - m1 ) ) 
		y = if y2 /= y1 then ( ( x1 + x2 - 2 * x ) / 2 * m1 ) + ( ( y1 + y2 ) / 2.0 ) else  ( ( x2 + x3 - 2 * x ) / 2 * m2 ) + ( ( y2 + y3 ) / 2.0 ) 
		r = sqrt $ ( x - x1 ) ^2 + ( y - y1 ) ^ 2 


--end of circle through three points 
--start of SPOJ code 


format::(Num a , Ord a ) => [[a]] -> [Point a]
format xs = map (\[x0 , y0] -> P x0 y0 ) xs 


readInt  ::( Num a , Read a ) =>   String -> a 
readInt  = read


solve :: ( Num a , Ord a , Floating a ) => [ Point a ] -> ( Point a , Point a , Point a , a ) 
solve [ P x0 y0 ] = ( P x0 y0 , P x0 y0 , P x0 y0 , 0 ) --in case of one point
solve [ P x0 y0 , P x1 y1 ] = (  P x0 y0 , P x0 y0 , P x0 y0 ,   sqrt $ ( x0 - x1 ) ^ 2 + ( y0 - y1 ) ^2    )  -- in case of two points the 
solve  xs = findPoints ( head ys ) ( head.tail $ ys ) ys where 
	ys  = convexHull xs  


final :: ( Num a , Ord a , Floating a ) => ( Point a , Point a , Point a , a ) -> a 
final ( u , v , t , w ) 
	| w == 0 = 0
        | and [ u == v , v == t ] = w 
	| otherwise = r where 
		( P x y , r )  = circlePoints u v t 


main = interact $   ( printf "%.2f\n" :: Double -> String ) .  final . solve . convexHull . format . map  ( map readInt . words ) . tail . lines   
