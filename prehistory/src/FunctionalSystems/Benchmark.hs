
module Main where

import Criterion.Main
import Performance

main :: IO ()
main = defaultMain [ bench "len0" $ whnf len0 [0..100000] ]
