{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module AATree where

data AATree a where
  Nil :: AATree a
  Node :: Int -> a -> AATree a -> AATree a -> AATree a

deriving instance  Show a => Show (AATree a)

skew :: AATree a -> AATree a
skew Nil = Nil
skew t @(Node _ _ Nil _) = t
skew (Node h v (Node lh lv lltree lrtree ) rtree)
  | h == lh = Node lh lv lltree (Node h v lrtree rtree)
skew t = t


split :: AATree a -> AATree a
split Nil = Nil
split (Node _ _ _ Nil) = Nil
split t @(Node _ _ _ (Node _ _ _ Nil)) = t
split (Node h v ltree (Node rh rv rltree rrtree@(Node rrh _ _ _)))
  | h == rrh = Node (rh + 1) rv (Node h v ltree rltree) rrtree
split t = t



insert :: (Ord a) => a -> AATree a -> AATree a
insert x Nil = Node 1 x Nil Nil
insert x (Node h v ltree rtree)
  | x < v = split . skew $ Node h v (insert x ltree) rtree
  | x > v = split . skew $ Node h v ltree (insert x rtree)
  | otherwise = Node h v ltree rtree


treefromList :: (Ord a) => [a] -> AATree a
treefromList = foldl (flip insert) Nil
