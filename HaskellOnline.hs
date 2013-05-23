
-- An online type checker for the Haskell programming language (see haskell.org).

-- Fix the typo to get rid of the compilation error.
val = filter (divides 3) [1..10]
    where divides n x = (x `mod` n) == 0

main = putStrLn (show val)
-- Click the button below the code editor to evaluate 'main'.

-- That was easy!
-- If you are an intermediate haskeller, try solving the following exercises, courtesy of Tony Morris.
-- Source: http://blog.tmorris.net/20-intermediate-haskell-exercises/
-- If you can get a solution to typecheck, there is a very good chance that it is correct.

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f ( Just x ) = Just ( f x )
  furry _ Nothing = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry = (.)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f ( EitherLeft ( Left a ) ) = EitherLeft ( Left ( f a ) )
  furry _ ( EitherLeft ( Right a ) ) = EitherLeft ( Right  a )


-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry _ ( EitherRight ( Left a ) ) = EitherRight ( Left a )
  furry f ( EitherRight ( Right a ) ) = EitherRight ( Right ( f a ) )

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f  = banana ( unicorn . f  ) 

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f x = concatMap f x 
  unicorn = \x -> [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f ( Just x ) = f x
  banana _ Nothing = Nothing
  unicorn = \x -> Just x

-- Exercise 9. Awesome problem. Love the State Monad
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g = \x -> let u = g x 
                     in f u x
  unicorn x = \_ -> x 

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft b ) where
  banana f ( EitherLeft t ) = case t of 
                                  Left a -> f a
                                  Right b -> EitherLeft ( Right b )
  unicorn x = EitherLeft ( Left x )

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight a ) where
   banana f ( EitherRight t ) = case t of 
                                  Left a -> EitherRight ( Left a )
                                  Right b ->  f b
   unicorn x = EitherRight ( Right x )

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean f = banana id f 

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple  x f =  undefined
-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs f = undefined -- unicorn .  banana f $ xs

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = undefined

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"

