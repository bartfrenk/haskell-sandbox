{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FunctionalSystems.Testing where

import           Data.Bits       (shiftR, (.&.))
import           Data.Char       (ord, chr)
import           Data.DeriveTH
import           Data.Word       (Word16)
import           System.Random

import           Test.HUnit
import           Test.QuickCheck hiding ((.&.))


encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise   = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

testOne :: Char -> Assertion
testOne char =
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar char))

testASCII :: Assertion
testASCII = mapM_ testOne ['\0'..'\127']

prop_encodeOne :: Char -> Bool
prop_encodeOne c = length (encodeChar c) == 1

prop_encodeBigChar :: BigChar -> Bool
prop_encodeBigChar (Big c) = length (encodeChar c) == 1

data Modal =
    Possible
  | Necessary
  | Contingent
  | Impossible
  deriving (Eq, Show)

newtype BigChar = Big Char
  deriving (Eq, Show, Random, Ord)

instance Arbitrary BigChar where
  arbitrary = choose (Big '\0', Big '\x10FFFF')
  shrink (Big c) = take 10 $ (Big . chr) `fmap` cs
    where f d = round $ (1.0 - d) * fromIntegral (ord c)
          cs = f `fmap` iterate ((0.5 :: Double) *) 1

derive makeArbitrary ''Modal

-- instance Arbitrary Modal where
--   arbitrary = elements [Possible, Necessary, Contingent, Impossible]


prop_encodeOne5 = do
  Big c <- arbitrary `suchThat` (< Big '\x10000')
  return $ length (encodeChar c) == 1
