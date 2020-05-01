{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
import Data.Kind (Type)


data Dual a  = Dual a a 
    deriving (Eq, Read, Show)

instance Num a => Num (Dual a) where
  (Dual f f') + (Dual g g') = Dual (f + g) (f' + g')
  (Dual f f') - (Dual g g') = Dual (f - g) (f' - g')
  (Dual f f') * (Dual g g') = Dual (f * g) (f * g' + g * f')
  fromInteger n = Dual (fromInteger n) 0
  abs (Dual f f') = Dual (abs f) (f' * signum f)
  signum (Dual f f') = Dual (signum f) 0



instance Fractional a => Fractional (Dual a) where
  (Dual f f') / (Dual g g') = Dual (f / g) ((f * g' - g * f')/ (g * g))
  recip (Dual f f') =  Dual (recip f) (f' / (f * f))
  fromRational n = Dual (fromRational n) 0



instance Floating a => Floating (Dual a) where 
  pi = Dual pi 0
  exp (Dual f f') = Dual (exp f) (f' * exp f)
  log (Dual f f') = Dual (log f) (f' / f)
  sin (Dual f f') = Dual (sin f) (cos f * f')
  cos (Dual f f') = Dual (cos f) (-sin f * f')
  tan (Dual f f') = Dual (tan f) (f' / (cos f * cos f)) 
  sqrt (Dual f f') = Dual (sqrt f) (f'/ (2 * sqrt f))
  asin (Dual f f') = Dual (asin f) (f' / sqrt (1 - f * f))
  acos (Dual f f') = Dual (acos f) (-f'/ sqrt (1 - f * f))
  atan (Dual f f') = Dual (atan f) (f' / (1 + f * f))



-- Extract the value `x` from `Dual x _`
evalDual :: Dual a -> a
evalDual (Dual x _) = x

-- Extract the derivative `x'` from `Dual _ x'`
diffDual :: Dual a -> a
diffDual (Dual _ x') = x'

d :: Num a => (Dual a -> Dual c) -> a -> c
d f = diffDual . f . flip Dual 1



data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat


data Vec :: Type -> Nat -> Type where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)