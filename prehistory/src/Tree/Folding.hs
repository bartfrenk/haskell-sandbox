module Tree.Folding where

import           Data.Set  (Set, fromList, member)
import           Data.Tree

example :: Tree Integer
example = Node 1 [
  Node 2 [
      Node 3 [],
      Node 4 []],
  Node 5 [
      Node 6 [],
      Node 7 [
          Node 8 [],
          Node 9 []]],
  Node 10 [
      Node 11 [],
      Node 12 []]]

-- | Given a list `xs` of descendants to be redrawn, compute the new list of
-- nodes to be redrawn when taking into consideration the parent `x`.
reducer :: Ord a => Set a -> Set a -> a -> [a] -> [a]
reducer changed flattened x xs
  | x `member` changed = x:xs
  | x `member` flattened && null xs = []
  | x `member` flattened = [x]
  | otherwise = xs

-- | result = [3, 5]
-- Note that the actual reducer is `reducer` applied to the sets that keep
-- track of the changed and flattened nodes. This works by currying, e.g.,
--
--     reducer changed flattened
--
-- takes an Integer and a list of Integers, and returns a list of Integers.
result :: [Integer]
result = foldr (reducer changed flattened) [] example
  where changed = fromList [3, 8]
        flattened = fromList [5, 10]



data ReplaceableList a = List [a] | Replace [a] deriving (Show)

instance Monoid (ReplaceableList a) where
  mempty = List []
  List [] `mappend` Replace _ = List []
  Replace _ `mappend` List [] = List []
  _ `mappend` Replace xs = List xs
  List xs `mappend` List ys = List (xs ++ ys)
  Replace xs `mappend` List _ = List xs

intoMonoid :: Ord a => Set a -> Set a -> a -> ReplaceableList a
intoMonoid changed flattened x =
  case (x `member` changed, x `member` flattened) of
    (_, True)      -> Replace [x]
    (True, False)  -> List [x]
    (False, False) -> List []

resultMonoid :: ReplaceableList Integer
resultMonoid = intoMonoid (fromList [3, 7, 5]) (fromList [1, 5]) `foldMap` example

