{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Misc where

import BasicPrelude
import Data.String.Conv

import qualified Data.Text as T
import Data.Text.Lazy.IO as TL

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

a :: String
a = "Gödel"

b :: BL.ByteString
b = "Einstein"

c :: B.ByteString
c = "Schrödinger"

(==~) :: (Eq a, StringConv b a) => a -> b -> Bool
(==~) x y = x == toS y
