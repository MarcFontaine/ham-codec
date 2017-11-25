----------------------------------------------------------------------------
-- |
-- Module      :  Codec.MsgJT
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only

module Codec.MsgJT
where

import Data.Word

newtype PlainText = PlainText {unPlainText:: String}
  deriving (Show,Eq,Ord)

newtype CallSign = CallSign {unCallsign:: String}
  deriving (Show,Eq,Ord)

type PackedMessage
  = (Word8,Word8,Word8,Word8,Word8,Word8
                 ,Word8,Word8,Word8,Word8,Word8,Word8)
data Message
  = PlainTextMessage PlainText
  | Blocks Block1 Block1 Block3
  deriving (Show,Eq,Ord)
    
data Block1
  =
    CQDX
  | CS CallSign
  | CQ    
  | CQE9 String
  | CQPrefix String
  | CQSuffix String
  | QRZ
  | QRZPrefix String
  | QRZSuffix String
  | CQFreq Word32
  | DE
  | DEPrefix String
  | DESuffix String   
  | Block1Other Word32
  deriving (Show,Eq,Ord)

data Block3
  = Grid Word32
  | Report Word32
  | ReportR Word32
  | RO
  | RRR
  | R73
  | Block3Other Word32
  deriving (Show,Eq,Ord)

packedMessageToList :: PackedMessage -> [Word8]
packedMessageToList
  (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11)
   = [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11]

packedMessageFromList :: [Word8] -> Maybe PackedMessage
packedMessageFromList
  [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11]
   = Just (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11)
packedMessageFromList _ = Nothing
