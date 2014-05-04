{-# LANGUAGE GADTs, RankNTypes #-}
import Control.Monad.Fix

fac :: ( Integer ->  Integer ) -> Integer -> Integer
fac  = \fact -> ( \n -> if n == 0 then 1 else n * fact ( n - 1 ) )


fact :: Integer -> Integer
fact = fix fac

data Tree where 
   Node :: a -> Tree -> Tree -> Tree
   Nil  :: Tree

{-
depth :: Tree -> Integer
depth Nil = 0
depth ( Node _ left right ) = 1 + max ( depth left ) (  depth right )
-}

depth :: ( Tree -> Integer ) -> Tree -> Integer
depth = \fun -> ( \x -> case x of 
                      Nil ->   0
                      Node _ left right ->  1 + max ( fun left ) ( fun right ) )

fib :: ( Integer -> Integer ) -> Integer -> Integer 
fib = \fun -> (\n -> case n of
                        0 -> 0
                        1 -> 1
                        _ -> fun ( n - 1 ) + fun ( n - 1 )  )


{-
sum' ::   forall a . Num a  =>  [ a ] -> a
sum' []  = 0
sum' ( x : xs ) = x + sum' xs 
-}

sum' :: forall a. Num a => ( [ a ] -> a ) -> [ a ] -> a
sum' = \fun -> ( \n -> case n of 
                          [] -> 0
                          ( x : xs ) -> x + fun xs  )



