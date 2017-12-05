
module Performance where

len0 :: [a] -> Int
len0 (_:xs) = 1 + len0 xs
len0 _ = 0
