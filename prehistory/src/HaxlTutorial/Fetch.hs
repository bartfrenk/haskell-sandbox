{-# LANGUAGE ExistentialQuantification #-}
module HaxlTutorial.Fetch where

import Data.IORef
import Data.Sequence
import Data.Foldable

data Request a -- application-specific type

data BlockedRequest =
  forall a . BlockedRequest (Request a) (IORef (FetchStatus a))

data FetchStatus a
  = NotFetched
  | FetchSuccess a

data Result a
  = Done a
  | Blocked (Seq BlockedRequest) (Fetch a)

newtype Fetch a = Fetch { unFetch :: IO (Result a) }

instance Functor Fetch where
  fmap f (Fetch m) = Fetch $ do
    m' <- m
    case m' of
      Done a -> return (Done (f a))
      Blocked br c -> return (Blocked br (fmap f c))

instance Applicative Fetch where
  pure = return

  Fetch f <*> Fetch x = Fetch $ do
    f' <- f
    x' <- x
    case (f', x') of
      (Done g, Done y) -> return (Done (g y))
      (Done g, Blocked br c) -> return (Blocked br (g <$> c))
      (Blocked br c, Done y) -> return (Blocked br (c <*> return y))
      (Blocked br1 c, Blocked br2 d) -> return (Blocked (br1 >< br2) (c <*> d))


instance Monad Fetch where

  return a = Fetch $ return (Done a)

  Fetch m >>= k = Fetch $ do
    r <- m
    case r of
      Done a -> unFetch (k a)
      Blocked br c -> return (Blocked br (c >>= k))

dataFetch :: Request a -> Fetch a
dataFetch request = Fetch $ do
  box <- newIORef NotFetched
  let br = BlockedRequest request box
  let cont = Fetch $ do
        FetchSuccess a <- readIORef box
        return (Done a)
  return (Blocked (singleton br) cont)

fetch :: [BlockedRequest] -> IO ()
fetch = undefined

runFetch :: Fetch a -> IO a
runFetch (Fetch h) = do
  r <- h
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch (toList br)
      runFetch cont
