{-# LANGUAGE TemplateHaskell #-}

module Lenses.Scratch where

import Control.Lens hiding (element)
import Test.QuickCheck

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makeLenses ''Atom

makeLenses ''Point

