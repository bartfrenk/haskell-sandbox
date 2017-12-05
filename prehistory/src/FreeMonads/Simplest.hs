{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad.Free

data ChoiceF a = Choice (Bool -> a) deriving Functor

type ChoiceM a = Free ChoiceF a

instance Show (ChoiceF a) where
  show (Choice _) = "Choice"

data WriteF a = Write String a deriving Functor

type Write a = Free WriteF a

logger' :: ChoiceF a -> Write (Bool -> a)
logger' ch@(Choice f) = liftF $ Write (show ch) f

logger :: ChoiceM a -> Write a
logger (Pure r) = return r
logger (Free ch@(Choice next)) =
  Free (Write (show ch) next)


main :: IO ()
main = undefined
