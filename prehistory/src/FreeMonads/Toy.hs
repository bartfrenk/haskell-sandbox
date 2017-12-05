{-# LANGUAGE DeriveFunctor #-}

module Toy where

import Control.Monad.Free

data ToyF a next = Env a (a -> next) | Output a next | Bell next | Done
  deriving (Functor)

type ToyM a = Free (ToyF a)

env :: String -> ToyM String a
env name = liftF (Env name id)

output :: String -> ToyM String ()
output msg = liftF (Output msg ())

data LogF next = Log String next
  deriving (Functor)

type LogM = Free LogF

data MachineF a next = Read (a -> next) | Write a next | Ping next | Stop
  deriving Functor

type MachineM a = Free (MachineF a)

program :: ToyM String ()
program = undefined
  
