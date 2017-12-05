module Day3 where

isOddSquare :: Integer -> Bool
isOddSquare = undefined

coordinates :: Integer -> (Int, Int)
coordinates n
  | n == 1 = (0, 0)
  | isOddSquare n = (0, 0)
  | otherwise = (1, 1)
