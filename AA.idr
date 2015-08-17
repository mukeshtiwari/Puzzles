%elim
data AATree : Type -> Type where
  Nil : AATree a
  Node : (l : Nat) -> (v : a) -> AATree a -> AATree a -> AATree a

total
skew : AATree a -> AATree a
skew Nil = Nil
skew t@(Node l v Nil y) = t
skew (Node l v (Node k x z w) y) with (decEq l k)
  skew (Node l v (Node l x z w) y) | (Yes Refl) =
    Node l x z (Node l v w y)
  skew (Node l v (Node k x z w) y) | (No contra) =
    Node l v (Node k x z w) y
skew t = t

total
split : AATree a -> AATree a
split Nil = Nil
split t@(Node l v x Nil) = t
split t@(Node l v x (Node k y z Nil)) = t
split (Node l v x (Node k y z (Node j w s t))) with (decEq l j)
  split (Node l v x (Node k y z (Node l w s t))) | (Yes Refl) =
    Node (succ k) y (Node l v x z) (Node l w s t)
  split (Node l v x (Node k y z (Node j w s t))) | (No contra) =
    (Node l v x (Node k y z (Node j w s t)))
split t = t

total
insert : (Ord a) => a -> AATree a -> AATree a
insert x Nil = Node 1 x Nil Nil
insert x (Node l v y z) with (compare x v)
  insert x (Node l v y z) | LT = split . skew $ Node l v (insert x y) z
  insert x (Node l v y z) | EQ = split . skew $ Node l v y z
  insert x (Node l v y z) | GT = split . skew $ Node l v y (insert x z)

total
treefromList : (Ord a) =>  List a -> AATree a
treefromList xs = foldl (flip insert) Nil xs

total
mint : AATree a -> a
mint Nil = ?rhs_1
mint (Node l v Nil y) = v
mint (Node l v x y) = mint x

total
maxt : AATree a -> a
maxt Nil = ?rhs_2
maxt (Node l v x Nil) = v
maxt (Node l v x y) = maxt y

total
level : AATree a -> Nat
level Nil = 0
level (Node l v x y) = l

total
declevel : AATree a -> AATree a
declevel Nil = Nil
declevel t@(Node l v x y) = declevel' t (1 + min (level x) (level y)) where
  declevel' Nil shouldbe = Nil
  declevel' (Node k z w s) shouldbe with (shouldbe < k)
    declevel' (Node k z w s) shouldbe | False = Node k z w s
    declevel' (Node k z w s) shouldbe | True with (shouldbe < level s)
      declevel' (Node k z w s) shouldbe | True | False = Node shouldbe z w s
      declevel' (Node k z w Nil) shouldbe | True | True = Node shouldbe z w Nil
      declevel' (Node k z w (Node j s t u)) shouldbe | True | True =
        Node shouldbe z w (Node shouldbe s t u)

total
skewRight : AATree a -> AATree a
skewRight Nil = Nil
skewRight (Node l v x y) = Node l v x (skew y)

total
skewRightRight : AATree a -> AATree a
skewRightRight Nil = Nil
skewRightRight t@(Node l v x Nil) = t
skewRightRight (Node l v x (Node k y z w)) =
  Node l v x (Node k y z (skew w))

total
splitRight : AATree a -> AATree a
splitRight Nil = Nil
splitRight (Node l v x y) = Node l v x (skew y)

total
rebalance : AATree a -> AATree a
rebalance = splitRight . split . skewRightRight . skewRight . skew . declevel

total
delete : (Ord a) => a -> AATree a -> AATree a
delete x Nil = Nil
delete x (Node l v y z) with (compare x v)
  delete x (Node l v y z) | LT = rebalance (Node l v (delete x y) z)
  delete x (Node l v y z) | GT = rebalance (Node l v y (delete x z))
  delete x (Node l v y z) | EQ with (y, z)
    delete x (Node l v y z) | EQ | (Nil, Nil) = Nil
    delete x (Node l v y z) | EQ | (Nil, _) =
      rebalance (Node l successor Nil (delete successor z))
      where successor = mint z
    delete x (Node l v y z) | EQ | (_, _) =
      rebalance (Node l predecessor (delete predecessor y) z)
      where predecessor = maxt y

correctness : (Ord a) => {x : a} -> delete x (insert x xs) = xs
correctness = ?correctness_rhs
