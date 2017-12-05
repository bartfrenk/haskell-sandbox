{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

import Control.Concurrent
import Control.Monad (when)

import Criterion.Main


import Control.Exception
import Data.Typeable

data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

data MySecondError = MySecondError String deriving (Show, Typeable)
instance Exception MySecondError

bla :: a
bla = error "asdsda"

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just action `catch` handler
  where handler e@(SomeException _) = do print e; return Nothing

test :: IO Int
test = do x <- throw (MyError "1")
          y <- throwIO (MyError "2")
          return $ y + y

pureCatcher :: a -> IO (Maybe a)
pureCatcher a = (a `seq` return (Just a))
                `catch` \(SomeException _) -> return Nothing

seqList :: [a] -> b -> b
seqList [] b = b
seqList (x:xs) b = seq x $ seqList xs b

seqList' xs b = foldr seq b xs

pingpong :: Bool -> Int -> IO ()
pingpong v n' = do
  mvc <- newEmptyMVar
  mvp <- newEmptyMVar
  let parent n | n > 0 = do putMVar mvc n
                            takeMVar mvp >>= parent
               | otherwise = return ()
      child = do n <- takeMVar mvc
                 putMVar mvp (n - 1)
                 child
  tid <- forkIO child
  _ <- parent n' `finally` killThread tid
  when v $ putStrLn ""

main :: IO ()
main = defaultMain [
  bench "thread switch test" $ whnfIO mybench
  ]
  where mybench = pingpong False 10000


wrap :: IO a -> IO a
wrap action = mask $ \unmask -> do
  mv <- newEmptyMVar
  _ <- forkIO $ (unmask action >>= putMVar mv) `catch`
                \e@(SomeException _) -> putMVar mv (throw e)
  takeMVar mv


data MutexState = Holder | NonHolder

data Mutex (a :: MutexState) where
  Create :: MVar () -> Mutex NonHolder
  WithHolder :: MVar () -> Mutex Holder

acquire :: Mutex NonHolder -> IO (Mutex Holder)
acquire (Create mvar) = do
  _ <- takeMVar mvar
  return $ WithHolder mvar

release :: Mutex Holder -> IO (Mutex NonHolder)
release (WithHolder mvar) = do
  putMVar mvar ()
  return $ Create mvar


data Character
  = Human { height :: Int, age :: Int }
  | Dog { height :: Int }


dog = Dog 20
