import Data.List
import Data.Complex

--horneR::Complex Double->[Complex Double]->Complex Double
--horneR x ps=foldl (\a b -> a*x+b) 0 ps

complexmulT x u v = ( (:+) ( u_0*x_0 - v_0*y_0 + u_1 ) ( x_0*v_0 + y_0*u_0 + v_1 ) ) where
	(x_0 :+ y_0) = x
	(u_0 :+ v_0) = u
	(u_1 :+ v_1) = v


-- u/v
complexdiV u v = ( (:+) ( (a*c+b*d)/(c*c+d*d) ) ( (b*c-a*d)/(c*c+d*d) ) ) where 
		(a :+ b) = u
		(c :+ d) = v

complexadD u v = ( (:+) (a+c) (b+d) ) where
		(a :+ b) = u
		(c :+ d) = v

complexsuB u v = ( (:+) (a-c) (b-d) ) where 
		 (a :+ b) = u
                 (c :+ d) = v


horneR::Complex Double->[Complex Double]->Complex Double
horneR x ps = foldl  (complexmulT x)  ( (:+) 0.0 0.0 ) ps



polydiFF::[Complex Double]->[Complex Double]
polydiFF ps = reverse.tail.zipWith compmultnuM  [0..].reverse $ ps 

compmultnuM n u = ( (:+) ( n*x_0 ) ( n*y_0 ) ) where
                (x_0 :+ y_0) = u

sqrT u= v where 
	(x_0 :+ y_0)=u
	a=sqrt( ( x_0 + sqrt( x_0*x_0 + y_0*y_0) )/2.0 )
	b=if y_0<0 then -sqrt( ( -x_0 + sqrt( x_0*x_0 + y_0*y_0) )/2.0 ) else sqrt( ( -x_0 + sqrt( x_0*x_0 + y_0*y_0) )/2.0 )
	v=( (:+) a b)	

sqrtneG n = ( (:+) 0.0 (sqrt.abs $ n) )


{--laguerrE ps= helplaguerrE ps (polydiFF ps) (polydiFF.polydiFF $ ps) 0 0 (genericLength ps) where 
   helplaguerrE p_0 p_1 p_2 x_0 cnt n
	| cnt>=100= x_0
	| otherwise = 
	   let 
		g=horneR x_0 p_1/horneR x_0 p_0
		h=g*g-( horneR x_0 p_2 / horneR x_0 p_0)
	        a=n/ max (g+sqrt--}
