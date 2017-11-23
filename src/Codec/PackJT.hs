----------------------------------------------------------------------------
-- |
-- Module      :  Codec.PackJT
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module contains function for packing a JT65 message into 72 bits
-- and for unpacking back.
-- It is a pure Haskell re-implementation of the original
-- packjt.f90 FORTRAN module from the wsjt-x project.
-- Packing and unpacking should be the inverse of each other
-- (also called a isomorphism).
-- This implementation uses a correct by construction approach to achieve the
-- isomorphism property.
-- The idea is to build the reversible codec by composing reversible blocks.
-- Instead of two separate function for packing and unpacking,
-- this module implements just one reversible function (aka isomorphism).
-- An extra advantage is that the coded is built of many
-- small blocks, that can be tested systematically and independent of each other.

module Codec.PackJT
where

import Data.Word
import Data.Bits

import Codec.PackISO
import Codec.MsgJT
import Codec.PackJTExtern (callJT65Code)

isoNum :: ISO Word32 Char
isoNum = charIso ['0'..'9']
isoApha :: ISO Word32 Char
isoApha = charIso ['A'..'Z']
isoAlphaBlank :: ISO Word32 Char
isoAlphaBlank = charIso $ ['A'..'Z'] ++ " "
isoAlphaNum :: ISO Word32 Char
isoAlphaNum = charIso $ ['0'..'9'] ++ ['A'..'Z']
isoAlphaNumBlank :: ISO Word32 Char
isoAlphaNumBlank =  charIso $ ['0'..'9'] ++ ['A'..'Z'] ++ " "
isoPlainTextChar :: ISO Word32 Char
isoPlainTextChar = charIso "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ +-./?"

checkPlainTextBit
  :: ISO (Word32,Word32,Word32)
         (Either (Word32,Word32,Word32) (Word32,Word32,Word32))
checkPlainTextBit = mkIso fw bw
  where
    fw (nc1, nc2, ng)
      = if testBit ng 15
           then return $ Right (nc1,nc2,clearBit ng 15)
           else return $ Left (nc1,nc2,ng)
    bw (Left l) = return l
    bw (Right (nc1, nc2, ng)) = return (nc1, nc2, setBit ng 15)

moveBits :: ISO (Word32, Word32, Word32) (Word32, Word32, Word32)
moveBits = (fw, bw)
  where
    fw (nc1,nc2,ng)
      = if save
           then return (nc1 `div` 2, nc2 `div` 2, ng + b1 + b2)
           else err "bitSwap fw"
        where
          b1 = if testBit nc1 0 then 0x08000 else 0
          b2 = if testBit nc2 0 then 0x10000 else 0
          save = (ng .&. 0xFFFF8000 == 0)
    bw (nc1,nc2,ng)
       = if save
            then return (2 * nc1 + b1, 2 * nc2 + b2, ng .&. 0x7fff)
            else err "bitswap rev"
         where  
           b1 = if testBit ng 15 then 1 else 0
           b2 = if testBit ng 16 then 1 else 0
           save =     (not $ testBit nc1 31)
                   && (not $ testBit nc2 31)
                   && (ng .&. 0xfffe0000 == 0)

pack12Words :: ISO PackedMsg (Word32,Word32,Word32)
pack12Words
  = mkIso (return . fw) bw
  where
    fw (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11) =
      (sl b0 22 + sl b1 16 + sl b2 10 + sl b3 4 + sr b4 2
      ,sl (b4 .&. 3) 26 + sl b5 20 + sl b6 14 + sl b7 8 + sl b8 2 + sr b9 4
      ,sl (b9 .&. 15) 12 + sl b10 6 + fromIntegral b11
      )
    bw (nc1,nc2,ng)
      = if not save then err (show ("pack12Words bw",(nc1,nc2,ng)))
        else return $                                 
      ( sr nc1 22
      , sr nc1 16
      , sr nc1 10
      , sr nc1 4
      , fromIntegral (4 * (nc1 .&. 15)) .|. (sr nc2 26 .&. 3)
      , sr nc2 20
      , sr nc2 14, sr nc2 8, sr nc2 2
      , fromIntegral (16 * (nc2 .&. 3)) .|. (sr ng 12 .&. 15)
      , sr ng 6
      , fromIntegral (ng .&. 63)
      )
      where
        save =   (nc1 .&. 0xF0000000) == 0
              && (nc2 .&. 0xF0000000) == 0
              && (ng  .&. 0xFFFF0000) == 0
    sr x n = fromIntegral (shiftR x n .&. 63)
    sl x n = shiftL (fromIntegral x) n
    
plainTextSplit :: ISO (String,String,String) PlainText
plainTextSplit = (fw,bw)
  where
    fw ([c0,c1,c2,c3,c4],[c5,c6,c7,c8,c9],[c10,c11,c12])
         = return $ PlainText [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12]
    fw _ = err "plainTextSplit fw"
    bw (PlainText [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12])
      = return ([c0,c1,c2,c3,c4],[c5,c6,c7,c8,c9],[c10,c11,c12])
    bw _ = err "plainTextSplit rw"

plainText :: ISO (Word32, Word32, Word32) PlainText
plainText
  = moveBits <.> tripple plainText5 plainText5 plainText3
      <.> plainTextSplit
  where
    plainText5
      = ( modDivChar 42 isoPlainTextChar
        $ modDivChar 42 isoPlainTextChar
        $ modDivChar 42 isoPlainTextChar
        $ modDivChar 42 isoPlainTextChar
        $ modDivChar 42 isoPlainTextChar nil
        ) <.> reverseIso
    plainText3
       = ( modDivChar 42 isoPlainTextChar
         $ modDivChar 42 isoPlainTextChar
         $ modDivChar 42 isoPlainTextChar nil
         ) <.>  reverseIso

callsign :: ISO Word32 CallSign
callsign =
  ( modDivChar 27 isoAlphaBlank
  $ modDivChar 27 isoAlphaBlank
  $ modDivChar 27 isoAlphaBlank
  $ modDivChar 10 isoNum
  $ modDivChar 36 isoAlphaNum
  $ modDivChar 37 isoAlphaNumBlank nil
  ) <.> reverseIso <.> isoCallSign
  where
    isoCallSign :: ISO String CallSign
    isoCallSign = mkIsoTotal CallSign unCallsign

    mapSwasiland :: ISO String String
    mapSwasiland = error "todo Swasiland"

between :: Word32 -> Word32 -> Word32 -> Bool
between a b x = a <= x && x <= b               
        
block1 :: ISO Word32 Block1
block1 = switch mkBlock1 cases
  where
    cases =
      ( Case ((<= nBase    ) , callsign , CS)
      $ casePoint (nBase + 1) CQ
      $ casePoint (nBase + 2) QRZ
      $ Case (between (nBase + 3) 267796944, freqIso , CQFreq)
      $ casePoint 267796945 DE
      $ Else (idIso, Block1Other)
      )
    mkBlock1 b = case b of
      CS cs -> injectCase0 $ Right cs
      CQ    -> injectCase1 $ Right ()
      QRZ   -> injectCase2 $ Right ()
      CQFreq freq -> injectCase3 $ Right freq
      DE -> injectCase4 $ Right ()
      Block1Other uncoded -> injectCase4 $ Left uncoded
    nBase = 37*36*10*27*27*27
    freqIso = mkIso (\x -> return $ x - nBase -3) (\x -> return $ x + nBase + 3)

locator :: ISO Word32 Block3
locator = switch mkBlock3 cases
  where
    cases = 
      ( Case ((<= ngBase)                          , idIso , Grid)
      $ Case (between (ngBase + 1) (ngBase + 31)   , idIso , Report)
      $ Case (between (ngBase + 32) (ngBase + 61)  , idIso , ReportR)
      $ casePoint (ngBase + 62) RO
      $ casePoint (ngBase + 63) RRR
      $ casePoint (ngBase + 64) R73
      $ Else (idIso, Block3Other)
      )
    mkBlock3 :: Block3 -> (Either (Either (Either (Either (Either (Either
                  Word32 () ) ()) ()) Word32) Word32) Word32)
    mkBlock3 x = case x of
      Grid g    -> injectCase0 $ Right g
      Report r  -> injectCase1 $ Right r
      ReportR r -> injectCase2 $ Right r
      RO        -> injectCase3 $ Right ()
      RRR       -> injectCase4 $ Right ()
      R73       -> injectCase5 $ Right ()
      Block3Other uncoded -> injectCase5 $ Left uncoded
    ngBase = 180*180
     
message :: ISO PackedMsg Message
message
  = pack12Words <.> checkPlainTextBit <.> eitherIso stdMsg plainText
    <.> mkIsoTotal toMsg fromMsg
 where
   stdMsg = tripple block1 block1 locator

   toMsg :: (Either (Block1, Block1, Block3) PlainText) -> Message
   toMsg (Left (b1,b2,b3) ) = Blocks b1 b2 b3
   toMsg (Right t) = PlainTextMessage t

   fromMsg :: Message -> (Either (Block1, Block1, Block3) PlainText)
   fromMsg (Blocks b1 b2 b3) = Left (b1,b2,b3)
   fromMsg (PlainTextMessage t) = Right t
   
testDecode :: String -> IO PackedMsg
testDecode str = do
  (sc,_) <- callJT65Code str
  print $ fwd message sc
  return sc
