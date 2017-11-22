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

type PackedMsg = (Word8,Word8,Word8,Word8,Word8,Word8
                 ,Word8,Word8,Word8,Word8,Word8,Word8)

data Message
  = PlainTextMessage PlainText
  | Blocks Block1 Block1 Block3
  deriving (Show,Eq,Ord)
    
data Block1
  = CS CallSign
  | CQ
  | QRZ
  | CQFreq Word32
  | DE
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
