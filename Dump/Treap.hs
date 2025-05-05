import Data.List
import System.Random
import Control.Monad
import Data.Bits

data Treap a = T Integer a ( Treap a ) ( Treap a ) | NIL deriving (Show , Eq , Ord )

searcH::(Ord a) => Treap a -> a -> Maybe a 
searcH NIL a' = Nothing
searcH ( T p a l r ) a'
   | a' < a = searcH l a'
   | a' > a = searcH r a'
   | otherwise = Just a'


prioritY::Treap a -> Integer
prioritY NIL = 0
prioritY ( T p _ _ _ ) = p


--clockwise rotation
rotateC::Treap a -> Treap a
rotateC ( T p a ( T p' a' x_l y_l ) y ) = T  p' a' x_l ( T p a y_l y )
rotateC ( T p a x y ) = T p a x y

--Anticlockwise rotation
rotateA::Treap a -> Treap a
rotateA ( T p a x ( T p' a' x_r y_r ) ) = T p' a' ( T p a x x_r ) y_r 
rotateA ( T p a x y ) = T p a x y


balancE::Treap a -> Treap a
balancE t@( T p a x y ) 
   | p < prioritY x = rotateC t
   | p < prioritY y = rotateA t
   | otherwise = t


inserT::(Ord a ) => ( Integer , a ) -> Treap a -> Treap a 
inserT ( p' , a' ) NIL = T p' a' NIL NIL
inserT ( p' , a' ) ( T p a x y ) 
   | a' <= a =  balancE $ T p a ( inserT ( p' , a' ) x ) y
   | otherwise = balancE $ T p a x ( inserT ( p' , a' ) y )


--Elegant deletion algorithm http://infoarena.ro/treapuri 
deletE::( Ord a ) => Treap a -> a -> Treap a
deletE NIL _ = NIL 
deletE t@( T p a x y ) a'
   | a' < a = T p a ( deletE x a' ) y
   | a' > a = T p a x ( deletE y a' )
   | otherwise = if x == NIL && y == NIL then NIL else if prioritY x > prioritY y then deletE ( rotateC t ) a' else deletE ( rotateA t  ) a'


getH::Treap a -> Integer
getH NIL = 0
getH ( T p a x y ) = 1 + max ( getH x ) ( getH y )


spliT::( Ord a ) => Treap a -> a -> ( Treap a , Treap a )
spliT NIL _ = ( NIL , NIL )
spliT t key = ( x , y ) where
	 T p a x y  = inserT ( ( shiftL ( 1::Integer ) 100 ) , key ) t
	

joiN::( Ord a ) => Treap a -> Treap a -> a -> Treap a
joiN NIL NIL _ = NIL
joiN NIL t _ = t
joiN t NIL _ = t
joiN t_1 t_2 key = t where 
	t' = T 0 key t_1 t_2 
	t = deletE t' key 


rInt:: String -> Integer
rInt = read 


main::IO ( )
main = do 
	  l <- fmap ( map rInt.words )  getLine 
	  --n <- fmap read getLine
	  --l <- replicateM n . randomRIO $ ( 1::Integer, 20::Integer )
	  m <- replicateM ( length l ) . randomRIO $ ( 1::Integer, shiftL (1::Integer ) 64 )
	  let list = zip m l
	      treap = foldr inserT NIL list
	  print treap
	  putStrLn "Height of treap "
	  print $ getH treap
	  putStrLn " input the key to split "
	  n <- fmap read getLine
	  let ( p , q ) = spliT treap  n
	  print p
	  print q
	  putStrLn "merging the two treaps "
	  print $ joiN p q n 
	  return ()
