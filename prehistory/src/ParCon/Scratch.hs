module ParCon.Scratch where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

delay :: Int -> IO ()
delay s = threadDelay (s * (1000000 :: Int))

