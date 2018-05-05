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

numChar:: String
numChar = ['0'..'9']
alphaChar :: String
alphaChar = ['A'..'Z']
alphaBlankChar :: String
alphaBlankChar = ['A'..'Z'] ++ " "
alphaNumChar :: String
alphaNumChar = ['0'..'9'] ++ ['A'..'Z']
alphaNumBlankChar :: String
alphaNumBlankChar =  ['0'..'9'] ++ ['A'..'Z'] ++ " "
plainTextChar :: String
plainTextChar = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ +-./?"

isoNum :: ISO Word32 Char
isoNum = charIso numChar
isoApha :: ISO Word32 Char
isoApha = charIso alphaChar
isoAlphaBlank :: ISO Word32 Char
isoAlphaBlank = charIso alphaBlankChar
isoAlphaNum :: ISO Word32 Char
isoAlphaNum = charIso alphaNumChar
isoAlphaNumBlank :: ISO Word32 Char
isoAlphaNumBlank =  charIso alphaNumBlankChar
isoPlainTextChar :: ISO Word32 Char
isoPlainTextChar = charIso plainTextChar


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
           else fwdError "bitSwap fw"
        where
          b1 = if testBit nc1 0 then 0x08000 else 0
          b2 = if testBit nc2 0 then 0x10000 else 0
          save = ng .&. 0xFFFF8000 == 0
    bw (nc1,nc2,ng)
       = if save
            then return (2 * nc1 + b1, 2 * nc2 + b2, ng .&. 0x7fff)
            else revError "bitswap rev"
         where  
           b1 = if testBit ng 15 then 1 else 0
           b2 = if testBit ng 16 then 1 else 0
           save =     not (testBit nc1 31)
                   && not (testBit nc2 31)
                   && (ng .&. 0xfffe0000 == 0)

pack12Words :: ISO PackedMessage (Word32,Word32,Word32)
pack12Words
  = mkIso (return . fw) bw
  where
    fw (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11) =
      (sl b0 22 + sl b1 16 + sl b2 10 + sl b3 4 + sr b4 2
      ,sl (b4 .&. 3) 26 + sl b5 20 + sl b6 14 + sl b7 8 + sl b8 2 + sr b9 4
      ,sl (b9 .&. 15) 12 + sl b10 6 + fromIntegral b11
      )
    bw (nc1,nc2,ng)
      = if not save then revError (show ("pack12Words bw",(nc1,nc2,ng)))
        else return
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
    sl x = shiftL (fromIntegral x)
    
plainTextSplit :: ISO (String,String,String) PlainText
plainTextSplit = (fw,bw)
  where
    fw ([c0,c1,c2,c3,c4],[c5,c6,c7,c8,c9],[c10,c11,c12])
         = return $ PlainText [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12]
    fw _ = fwdError "plainTextSplit fw"
    bw (PlainText [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12])
      = return ([c0,c1,c2,c3,c4],[c5,c6,c7,c8,c9],[c10,c11,c12])
    bw _ = revError "plainTextSplit rw"

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

callSign :: ISO Word32 CallSign
callSign =
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
block1 = alternatives [
   altPoint 90328121 CQDX
  ,interval (258024473,258043373) <.> (option $ shiftVal 258024473 )
     <.> option hackCQ <.> rightGuard CQE9 getCQE9  --overlapps Plaintext
  ,interval (0,base) <.> option callSign <.> rightGuard CS getCS
  ,altPoint (base + 1) CQ
  ,altPoint (base + 2) QRZ
  ,interval (base + 3, base + 1003) <.> idIso <.> rightGuard CQFreq getCQFreq
  ,interval (262178563 ,264002071) <.> option prefix
     <.> rightGuard CQPrefix getCQPrefix
  ,interval (264002072 , 265825580) <.> option prefix
     <.> rightGuard QRZPrefix getQRZPrefix
  ,interval (265825581 , 267649089) <.> option prefix
     <.> rightGuard DEPrefix getDEPrefix
  ,interval (267649090 , 267698374) <.> option suffix
     <.> rightGuard CQSuffix getCQSuffix
  ,interval (267698375 , 267747659) <.> option suffix
     <.> rightGuard QRZSuffix getQRZSuffix
  ,interval (267747660 , 267796944) <.> option suffix
     <.> rightGuard DESuffix getDESuffix   
  ,altPoint 267796945 DE
  ,option idIso <.> rightGuard Block1Other getBlock1Other  
  ]
  where
    base = 37*36*10*27*27*27
    hackCQ = callSign <.> mkIsoTotal fwdE9 revE9
    fwdE9 (CallSign  [' ','E','9',a,b,' ']) = [a,b]
    fwdE9 _ = error "fwdE9"
    revE9 [a,b] = CallSign  [' ','E','9',a,b,' ']
    revE9 _ = error "revE9"

prefix :: ISO Word32 String 
prefix =
  ( modDivChar 37 isoAlphaNumBlank
  $ modDivChar 37 isoAlphaNumBlank
  $ modDivChar 37 isoAlphaNumBlank
  $ modDivChar 37 isoAlphaNum nil
  ) <.> reverseIso

suffix :: ISO Word32 String 
suffix =
  ( modDivChar 37 isoAlphaNumBlank
  $ modDivChar 37 isoAlphaNumBlank
  $ modDivChar 37 isoAlphaNum nil
  ) <.> reverseIso



shiftVal :: Num a => a -> ISO a a
shiftVal offset
  = mkIsoTotal (+ offset) (\x -> x - offset)

    
locator :: ISO Word32 Block3
locator = alternatives
   [
     interval (0,base) <.> option splitGrid <.> mkIso kaFw kaRev
        <.> toReport <.> rightGuard ExtReport getExtReport
    ,interval (0,base) <.> option splitGrid <.> mkIso laFw laRev
        <.> toReport <.> rightGuard ExtReportR getExtReportR
    ,interval (0,base) <.> option (modDiv 180) <.> rightGuard Grid getGrid
    ,interval (base +2, base + 31)
        <.> oneBased <.> rightGuard Report getReport
    ,interval (base +32, base + 61)
               <.> oneBased <.> rightGuard ReportR getReportR
    ,altPoint (base + 62) RO
    ,altPoint (base + 63) RRR
    ,altPoint (base + 64) R73
    ,idIso <.> rightGuard Block3Other getBlock3Other
    ]
  where
    base = 180*180
    oneBased = option $ shiftVal 1
    splitGrid = modDiv 180 <.> pair (modDiv 10) (modDiv 10)

    kaFw (Just ((x,0),(y,7))) = return $ Just (x,y)
    kaFw _ = return Nothing
    kaRev (Just (x,y)) = return $ Just ((x,0),(y,7))
    kaRev _ = return Nothing
    laFw (Just ((x,0),(y,6))) = return $ Just (x,y)
    laFw _ = return Nothing
    laRev (Just (x,y)) = return $ Just ((x,0),(y,6))
    laRev _ = return Nothing
    toReport = option $ mkIsoTotal repFw repRev
    repFw (x,y) = 40 + fromIntegral x - fromIntegral y *10
    repRev x = (fromIntegral $ x `mod` 10, fromIntegral $ 4- (x `div` 10))

message :: ISO PackedMessage Message
message
  = pack12Words <.> checkPlainTextBit <.> eitherIso stdMsg plainText
    <.> mkIsoTotal toMsg fromMsg
 where
   stdMsg = tripple block1 block1 locator

   toMsg :: Either (Block1, Block1, Block3) PlainText -> Message
   toMsg (Left (b1,b2,b3) ) = Blocks b1 b2 b3
   toMsg (Right t) = PlainTextMessage t

   fromMsg :: Message -> Either (Block1, Block1, Block3) PlainText
   fromMsg (Blocks b1 b2 b3) = Left (b1,b2,b3)
   fromMsg (PlainTextMessage t) = Right t
