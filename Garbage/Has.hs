{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
import Data.Kind


data Dual a  = Dual a a 
    deriving (Eq, Read, Show)

instance Num a => Num (Dual a) where
  (Dual u u') + (Dual v v') = Dual (u + v) (u' + v')
  (Dual u u') - (Dual v v') = Dual (u - v) (u' - v')
  (Dual u u') * (Dual v v') = Dual (u * v) (u * v' + v * u')
  fromInteger n = Dual (fromInteger n) 0
  abs (Dual u u') = Dual (abs u) (u' * signum u)
  signum (Dual u u') = Dual (signum u) 0



instance Fractional a => Fractional (Dual a) where
  (Dual u u') / (Dual v v') = Dual (u / v) ((u * v' - v * u')/ (v * v))
  fromRational n = Dual (fromRational n) 0


  


data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat


data Vec :: Type -> Nat -> Type where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)