module Codec.Test
where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Word

import Codec.JT65
--import Codec.WrapKarn

{-
newtype Packed = Packed [Word8]
  deriving Show
           
instance Arbitrary Packed
  where
    arbitrary = fmap Packed $ vectorOf 12 $ choose (0,63)

testEncodeRS :: Packed -> Property
testEncodeRS (Packed msg) = monadicIO $ do
  code <- run (Codec.WrapKarn.encodeRS msg)
  assert (code == Codec.JT65.encodeRS msg)
-}
