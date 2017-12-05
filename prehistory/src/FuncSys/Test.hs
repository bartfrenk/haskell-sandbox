{-# LANGUAGE DeriveFunctor #-}

module FuncSys.Test where

data F a = F (a -> Bool)
