----------------------------------------------------------------------------
-- |
-- Module      :  Codec.PackJTExtern
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module calls the jt65code command line utilty from the wsjt-x project.
-- Its is useful as an alternative to standalone Haskell implemenation
-- and for compatibilty testing of the orginal and the Haskell reimplementaion.

module Codec.PackJTExtern
where
import System.Process (readProcess)
import Data.Word

callJT65Code :: String
             -> IO ((Word8,Word8,Word8,Word8,Word8,Word8,
                     Word8,Word8,Word8,Word8,Word8,Word8),[Word8])
callJT65Code input = do
  ret <- readProcess "jt65code" [input] ""
  let
    [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11] 
      = map read $ words $ drop 30 (lines ret !! 4)
    shortCode = (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11)
    (z:y:x:_) = reverse $ lines ret
    longCode = map read $ words $ concat [x,y,z]
  return (shortCode, longCode)
