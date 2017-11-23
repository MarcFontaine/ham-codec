----------------------------------------------------------------------------
-- |
-- Module      :  Codec.JT65
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only

module Codec.JT65
where
import Data.Time
import Data.Word
import Data.Bits
import Data.Ratio
import qualified Data.Array as Array
import Data.Array (Array)
import Data.Tuple (swap)
import Codec.PackJTExtern

type Frequency = Rational

jt65AEncodeExtern :: String -> IO [Frequency]
jt65AEncodeExtern input = do
  (_,symbols) <- callJT65Code input
  return $ encodeSymbols symbols

jt65AEncode :: String -> IO [Frequency]
jt65AEncode= jt65AEncodeExtern
encodeSymbols :: [Word8] -> [Frequency]
encodeSymbols l
  = map (toFrequency A) $ mixinSync l


-- todo fix this refactor 
jt65ASchedule :: String -> IO [(DiffTime,Frequency)]
jt65ASchedule input = do
  frequencies <- jt65AEncode input
  nextSlot <- nextSlotTime 1 --- s second guardtime ??
  let
    symbolTimes = map ((+) nextSlot) jt65SymbolStartTimes
  return $ zip symbolTimes frequencies

nextSlotTime :: DiffTime -> IO DiffTime
nextSlotTime guardTime = do
  now <- fmap utctDayTime getCurrentTime 
  let
    t :: Integer
    t = ceiling (( now + guardTime ) / 60) * 60
  return $ fromIntegral t

jt65SymbolStartTimes :: [DiffTime]
jt65SymbolStartTimes = take 126 $ map fromRational [0 , (4096 % 11025) ..]
  
data Symbol = Sync | Tone Word8
  deriving (Show,Eq)

mixinSync :: [Word8] -> [Symbol]
mixinSync l = worker jt65MasterSyncVector l
  where
    worker ('1':rest) msg   = Sync   : worker rest msg
    worker ('0':rest) (h:t) = Tone h : worker rest t
    worker [] [] = []
    worker _ _ = error $ show ("mixinSync something wrong:",l)


jt65MasterSyncVector :: String
jt65MasterSyncVector
  =    "100110001111110101000101100100011100111101101111000110101011001"
   ++  "101010100100000011000000011010010110101010011001001000011111111"


data SubMode = A | B | C deriving (Show,Eq)

toFrequency :: SubMode -> Symbol -> Frequency
toFrequency mode sym = case sym of
  Sync   -> baseTone
  Tone s -> baseTone + spacing * (fromIntegral s + 2) * m
  where
    baseTone = 12705 % 10
    spacing  = 26917 % 10000
    m = case mode of
      A -> 1
      B -> 2
      C -> 4


data Direction = Forward | Reverse
  deriving (Show,Eq)

interleaveSymbols :: Direction -> [Word8] -> [Word8]
interleaveSymbols dir syms
  = Array.elems $ Array.ixmap boundsOut swapIx $ Array.listArray boundsIn syms
  where
    (boundsOut,boundsIn) = case dir of
      Forward -> bounds
      Reverse -> swap bounds
    bounds = (((0,0),(6,8))  ,  ((0,0),(8,6)))
    swapIx :: (Int,Int) -> (Int,Int)
    swapIx = swap

grayCode :: Direction -> Word8 -> Word8
grayCode dir sym =
  case dir of
    Forward -> grayTable Array.! sym
    Reverse -> invTable Array.! sym
  where
    grayTable = Array.array (0,63) (zip [0..63] grayList)
    invTable =  Array.array (0,63) (zip grayList [0..63])
    
    grayList = [0,1,3,2,6,7,5,4,12,13,15,14,10,11,9,8
                 ,24,25,27,26,30,31,29,28,20,21,23,22,18,19,17,16,48
                 ,49,51,50,54,55,53,52,60,61,63,62,58,59,57,56,40,41
                 ,43,42,46,47,45,44,36,37,39,38,34,35,33,32]


encodeRS :: [Word8] -> [Word8]
encodeRS syms = reverse parity ++ syms
  where
    parity = foldl iter (replicate 51 0) (reverse syms)
    iter bb sym =
      if feedback /= 63
         then zipWith op btail  genPolyL ++ [alphaTo (feedback + genPoly0)]
         else btail ++ [0]
      where
        feedback = indexOf (sym `xor` (head bb))
        btail = tail bb
        op b p = b `xor` alphaTo (feedback + p)
        genPoly0 = head genPoly
        genPolyL = reverse genPoly

indexOf :: Word8 -> Word8
indexOf = (Array.! ) table
  where
    table :: Array Word8 Word8
    table = Array.listArray (0,63)
           [63,0,1,6,2,12,7,26,3,32,13,35,8,48,27,18,4,24,33,16,14,52,36
          ,54,9,45,49,38,28,41,19,56,5,62,25,11,34,31,17,47,15,23,53,51
          ,37,44,55,40,10,61,46,30,50,22,39,43,29,60,42,21,20,59,57,58]

alphaTo :: Word8 -> Word8
alphaTo x = table Array.! (x `mod` 63)
  where
    table = Array.listArray (0,62)
           [1,2,4,8,16,32,3,6,12,24,48,35,5,10,20,40,19,38,15,30,60,59,53
           ,41,17,34,7,14,28,56,51,37 ,9,18,36,11,22,44,27,54,47,29,58,55
           ,45,25,50,39,13,26,52,43,21,42,23,46,31,62,63,61,57,49,33]

genPoly :: [Word8]
genPoly = [42,36,57,12,9,41,22,21,27,39,18,41,52,19,39,21,4,59,27
          ,15,51,10,37,51,58,36,8,37,37,30,10,58,29,48,24,39,0,25
          ,12,52,48,32,60,55,56,1,27,2,12,1,50]
