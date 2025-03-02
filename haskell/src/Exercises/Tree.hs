module Exercises.Tree (
  empty,
  singleton,
  insert,
  member,
  lookupT,
) where

type Key = String
data Tree v = Leaf | Node Key v (Tree v) (Tree v)

empty :: Tree a
empty = Leaf

singleton :: Key -> a -> Tree a
singleton k v = Node k v Leaf Leaf

insert :: (Key, a) -> Tree a -> Tree a
insert m Leaf = uncurry singleton m
insert m@(k, v) (Node l u a b)
  | k > l = Node l u a (insert m b)
  | k < l = Node l u (insert m a) b
  | otherwise = Node k v a b

member :: Key -> Tree a -> Bool
member _ Leaf = False
member k (Node l _ a b) = (k == l) || member k a || member k b

lookupT :: (Ord a) => Key -> Tree a -> Maybe a
lookupT _ Leaf = Nothing
lookupT k (Node l v a b)
  | k == l = Just v
  | otherwise = max <$> lookupT k a <*> lookupT k b
