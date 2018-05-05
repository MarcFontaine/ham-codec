----------------------------------------------------------------------------
-- |
-- Module      :  Codec.PackISO
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
{-# Language GADTs, RankNTypes #-}
module Codec.PackISO
where
import Control.Monad
import qualified Data.Map as Map
import Data.Word
import Lens.Micro

type ES a = Either String a

-- | ISO a b is a isomorphism between a b.
type ISO a b = (a -> ES b, b -> ES a)

fwd :: ISO a b -> (a -> ES b)
fwd = fst

rev :: ISO a b -> (b -> ES a)
rev = snd

roundTripFwdRev :: ISO a b -> a -> ES a
roundTripFwdRev iso a = fwd iso a >>= rev iso

roundTripRevFwd :: ISO a b -> b -> ES b
roundTripRevFwd iso b = rev iso b >>= fwd iso

mkIso :: (a -> ES b) -> (b -> ES a) -> ISO a b
mkIso x y = (x,y)

mkIsoTotal :: (a -> b) -> (b -> a) -> ISO a b
mkIsoTotal x y = (return . x, return . y)

fwdError :: String -> ES x
fwdError = Left

revError :: String -> ES x
revError = Left

-- | Identity isomorphism.
idIso :: ISO a a
idIso = mkIso return return

point :: (Eq a, Eq b) => a -> b -> ISO a b
point code val = mkIso fp bp
  where
    fp x = if x == code then return val  else fwdError "point fwd"
    bp x = if x ==  val then return code else revError "point rev"

nil :: ISO Word32 [a]
nil = mkIso
  (\x -> if x==0 then return [] else fwdError "nil not 0")
  (\[] -> return 0)

unit :: (Eq a, Num a) => ISO a ()
unit = mkIso
  (\x -> if x==0 then return () else fwdError "unit not 0")
  (\() -> return 0)

-- | Chain two isomorphism
chain :: ISO a b -> ISO b c -> ISO a c
chain (ff, fb) (gf, gb)
  = mkIso (ff >=> gf) (gb >=> fb)

-- | infix sytax for chain
(<.>) :: ISO a b -> ISO b c -> ISO a c
(<.>) = chain


charIso :: String -> ISO Word32 Char
charIso chars = mkIso fw bw
  where
    fw x = case Map.lookup x fwMap of
      Nothing -> fwdError $ show ("charIso  fw bad char",x)
      Just v -> return v
    bw x = case Map.lookup x bwMap of
      Nothing -> revError $ show ("charIso  rev bad code",x)
      Just v -> return v                   
    fwMap = Map.fromList $ zip [0..] chars
    bwMap = Map.fromList $ zip chars [0..]

pair :: ISO a b -> ISO c d -> ISO (a,c) (b,d)
pair (f1,b1) (f2,b2) = mkIso fp bp
  where
    fp (a,b) = do
      x <- f1 a
      y <- f2 b
      return (x,y)
    bp (a,b) = do
      x <- b1 a
      y <- b2 b
      return (x,y)

eitherIso :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
eitherIso (fl,bl) (fr,br) = mkIso fp bp
  where
    fp (Left l) = Left <$> fl l
    fp (Right r) = Right <$> fr r
    bp (Left l) = Left <$> bl l
    bp (Right r) = Right <$> br r

tripple :: ISO a b -> ISO c d -> ISO e f -> ISO (a,c,e) (b,d,f)
tripple (f1,b1) (f2,b2) (f3,b3) = (fp,bp)
  where
    fp (a,b,c) = do
      x <- f1 a
      y <- f2 b
      z <- f3 c
      return (x,y,z)
    bp (a,b,c) = do
      x <- b1 a
      y <- b2 b
      z <- b3 c
      return (x,y,z)

modDiv :: Word32 -> ISO Word32 (Word32, Word32)
modDiv n = (fw,bw)
  where
    fw x = return (x `mod` n, x `div` n)
    bw (a,b) = if a < n
       then return (n * b + a)
       else revError $ show ("modDiv rev out of Range",a)

concChar :: ISO (Char,String) String
concChar = (fw,bw)
  where
    fw (c,r) = return (c:r)
    bw [] = revError "concChar rew empty list"
    bw (c:r) = return (c,r)

modDivChar :: Word32 -> ISO Word32 Char
         -> ISO Word32 String -> ISO Word32 String
modDivChar n chrCoder rest
  = modDiv n <.> pair chrCoder rest  <.> concChar

reverseIso :: ISO String String
reverseIso = (return . reverse, return . reverse)

ifte :: ((a -> Bool), ISO a b) -> ISO a c -> ISO a (Either c b)
ifte  (cond,t) e = mkIso fw bw
  where
    fw x = if cond x
              then Right <$> fwd t x
              else Left <$> fwd e x
    bw (Left x) = rev e x
    bw (Right x) = rev t x

altPoint :: (Num a, Ord a, Eq b) => a -> b -> ISO (Maybe a) (Maybe b)
altPoint c v
  = interval (c,c) <.> option unit <.> rightEq v

option :: ISO a b -> ISO (Maybe a) (Maybe b)
option iso = mkIso fw rv
  where
    fw (Just x) = Just <$> fwd iso x
    fw Nothing = return Nothing
    rv (Just x) = Just <$> rev iso x
    rv Nothing = return Nothing

interval :: (Num a, Ord a) => (a,a) -> ISO (Maybe a) (Maybe a)
interval (a,b) = mkIso fw rw
  where
    fw Nothing = return Nothing
    fw (Just x)  = if a <= x && x <= b
             then return $ Just (x-a)
             else return Nothing
    rw Nothing  = return Nothing
    rw (Just v) = let x'= a+v in
       if a <= x' && x' <= b
          then return $ Just x'
          else return Nothing
                    

rightGuard :: ( a->b ) -> Lens.Micro.Traversal' b a -> ISO (Maybe a) (Maybe b)
rightGuard d tr = mkIso fw rv
  where
    fw Nothing = return Nothing
    fw (Just x) = return $ Just $ d x
    rv Nothing = return Nothing
    rv (Just x) = return $ (Lens.Micro.^?) x tr

rightEq :: Eq a => a -> ISO (Maybe ()) (Maybe a)
rightEq v = mkIso fw rv
  where
    fw Nothing = return Nothing
    fw (Just ()) = return $ Just v
    rv Nothing = return Nothing
    rv (Just x) = if x == v
                    then return $ Just ()
                    else return Nothing             
    
alternatives :: [ISO (Maybe a) (Maybe b)] -> ISO a b
alternatives cases = mkIso (alt fwd cases) (alt rev cases)
  where
    alt _   []    _ = Left "alternatives error no match"
    alt dir (h:t) x = do
      v <- dir h $ Just x
      case v of
        Just v' -> return v'
        Nothing -> alt dir t x
