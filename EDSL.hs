{-# LANGUAGE GADTs #-}

data Expr where 
  Lit :: Integer -> Expr
  Add :: Expr -> Expr -> Expr
  Sub :: Expr -> Expr -> Expr
  Mul :: Expr -> Expr -> Expr
  Var :: String -> Expr
  Lam :: String -> Expr
  deriving ( Show, Eq )

instance Num Expr where
  fromInteger n = Lit n 
  eone + etwo = Add eone etwo
  eone - etwo = Sub eone etwo
  eone * etwo = Mul eone etwo


evaluate :: Expr ->  Integer
evaluate ( Lit n ) = n
evaluate ( Add a b ) = evaluate a + evaluate b
evaluate ( Sub a b ) = evaluate a - evaluate b
evaluate ( Mul a b ) = evaluate a * evaluate b

f :: Expr -> Expr
f x =  x + 1

