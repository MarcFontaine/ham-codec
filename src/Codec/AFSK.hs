module Codec.AFSK
where

import Data.Ratio
import Data.Vector.Storable as Vector
import Data.List (mapAccumL)

type AF = Vector Double

{-
-- Double :: ca 1s

-- Rational :: too slow --> idea use lazyness compute signal on the fly
-- use some kind of buffering

afsk :: Int -> [(Rational,Int)] -> AF
afsk rate tones
  = Vector.map (\p -> sin (pi2*p)) $ accumRamps rate tones
  where pi2=2*pi

accumRamps :: Int -> [(Rational,Int)] -> AF
accumRamps rate tones
  = Vector.concat $ snd 
     $ mapAccumL (\p (f,cnt) -> ramps rate (fromRational f) p cnt) 0 tones

ramps :: Int -> Double -> Double -> Int -> (Double, AF)
ramps rate frequency inPhase count = (outPhase,vec)
  where
    vec = generate count phase
    outPhase = phase count
    phase :: Int -> Double
    phase t = snd $ pF (inPhase + (fromIntegral t / fromIntegral rate) * frequency)
    
    pF :: Double -> (Integer,Double)
    pF = properFraction

-}

afsk :: Int -> [(Rational,Int)] -> AF
afsk rate tones
  = Vector.map (\p -> sin (pi2*p)) $ accumRamps rate tones
  where pi2=2*pi

accumRamps :: Int -> [(Rational,Int)] -> AF
accumRamps rate tones
  = Vector.concat $ snd 
     $ mapAccumL (\p (f,cnt) -> ramps rate f p cnt) 0 tones

ramps :: Int -> Rational -> Rational -> Int -> (Rational, AF)
ramps rate frequency inPhase count = (outPhase,vec)
  where
    vec = generate count (fromRational . phase)
    outPhase = phase count
    phase :: Int -> Rational
    phase t = snd $ pF (inPhase + (fromIntegral t % fromIntegral rate) * frequency)
    
    pF :: Rational -> (Integer,Rational)
    pF = properFraction


{-
 -- accurate only for multipes of 11025 Hz (==44100Hz)
jt65AF :: Int -> Rational -> [Word8] -> AF
jt65AF rate baseFreq symbols
  = afsk rate $ zip fs ls
  where
    fs = map ((+) baseFreq) $ encodeSymbols symbols
    ls = repeat $ (4096*rate) `div` 11025

jt65AEncodeAF :: Frequency -> String -> IO AF
jt65AEncodeAF f input = do
  symbols <- callJT65Code input
  return $ jt65AF 44100 f symbols
-}
