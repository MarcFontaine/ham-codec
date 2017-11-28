----------------------------------------------------------------------------
-- |
-- Module      :  Codec.MsgJT
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
{-# Language TemplateHaskell #-}
module Codec.MsgJT
where

import Lens.Micro.TH  
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
  = CQDX
  | CS {_getCS :: CallSign}
  | CQ    
  | CQE9 {_getCQE9 :: String}
  | CQPrefix {_getCQPrefix :: String}
  | CQSuffix {_getCQSuffix :: String}
  | QRZ
  | QRZPrefix {_getQRZPrefix :: String}
  | QRZSuffix {_getQRZSuffix :: String}
  | CQFreq {_getCQFreq :: Word32}
  | DE
  | DEPrefix {_getDEPrefix :: String}
  | DESuffix {_getDESuffix :: String}
  | Block1Other {_getBlock1Other :: Word32}
  deriving (Show,Eq,Ord)

data Block3
  = Grid {_getGrid :: (Word32, Word32)}
  | Report {_getReport :: Word32}
  | ReportR {_getReportR :: Word32}    
  | RO
  | RRR
  | R73
  | ExtReport  {_getExtReport :: Int}
  | ExtReportR {_getExtReportR :: Int}    
  | Block3Other {_getBlock3Other :: Word32}
  deriving (Show,Eq,Ord)

makeLenses ''Block3
makeLenses ''Block1
           
packedMessageToList :: PackedMessage -> [Word8]
packedMessageToList
  (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11)
   = [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11]

packedMessageFromList :: [Word8] -> Maybe PackedMessage
packedMessageFromList
  [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11]
   = Just (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11)
packedMessageFromList _ = Nothing
