module Day3 where

import Math.NumberTheory.Powers.Squares

exactOddSquareRoot :: Integer -> Maybe Integer
exactOddSquareRoot n = case exactSquareRoot n of
  Just k -> if k `mod` 2 == 1 then Just k else Nothing
  Nothing -> Nothing

coordinates :: Integer -> (Integer, Integer)
coordinates n
  | n == 1 = (0, 0)
  | otherwise = case exactOddSquareRoot n of
      Just k ->
        let c = (k - 1) `div` 2
        in (c, -c)
      Nothing ->
        let (x, y) = coordinates (n - 1)
        in descend (x, y)

descend :: (Integer, Integer) -> (Integer, Integer)
descend (x, y)
  | x == y && x > 0 = (x - 1, y)
  | x == y && x < 0 = (x + 1, y)
  | x == -y && x > 0 = (x + 1, y)
  | x == -y && x < 0 = (x, y - 1)
  | abs x > abs y && x > 0 = (x, y + 1)
  | abs x > abs y && x < 0 = (x, y - 1)
  | abs x < abs y && y > 0 = (x - 1, y)
  | abs x < abs y && y < 0 = (x + 1, y)
  | otherwise = error "cannot happen"

distance :: Integer -> Integer
distance n = abs x + abs y
  where (x, y) = coordinates n


