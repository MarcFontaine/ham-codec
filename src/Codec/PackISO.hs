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
{-# Language GADTs #-}
module Codec.PackISO
where
import Control.Monad
import qualified Data.Map as Map
import Data.Word

type ES a = Either String a

-- | ISO a b is a isomorphism between a b.
type ISO a b = (a -> ES b, b -> ES a)

fwd :: ISO a b -> (a -> ES b)
fwd = fst

rev :: ISO a b -> (b -> ES a)
rev = snd

mkIso :: (a -> ES b) -> (b -> ES a) -> ISO a b
mkIso x y = (x,y)

mkIsoTotal :: (a -> b) -> (b -> a) -> ISO a b
mkIsoTotal x y = (return . x, return . y)

err :: String -> ES x
err msg = Left msg

-- | Identity isomorphism.
idIso :: ISO a a
idIso = mkIso return return

point :: (Eq a, Eq b) => a -> b -> ISO a b
point code val = mkIso fp bp
  where
    fp x = if x == code then return val  else err "point fwd"
    bp x = if x ==  val then return code else err "point rev"

nil :: ISO Word32 [a]
nil = (\x -> if x==0 then return [] else err "nil not 0"
      ,\[] -> return 0)

unit :: ISO Word32 ()
unit = (\x -> if x==0 then return () else err "unit not 0"
      ,\() -> return 0)

-- | Chain two isomorphism
chain :: ISO a b -> ISO b c -> ISO a c
chain (ff, fb) (gf, gb)
  = mkIso (ff >=> gf) (gb >=> fb)

-- | infix sytax for chain
(<.>) :: ISO a b -> ISO b c -> ISO a c
(<.>) = chain


charIso :: [Char] -> ISO Word32 Char
charIso chars = mkIso fw bw
  where
    fw x = case Map.lookup x fwMap of
      Nothing -> err $ show ("charIso  fw bad char",x)
      Just v -> return v
    bw x = case Map.lookup x bwMap of
      Nothing -> err $ show ("charIso  rev bad code",x)
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
    fp (Left l) = fl l >>= return . Left
    fp (Right r) = fr r  >>= return . Right
    bp (Left l) = bl l >>= return . Left
    bp (Right r) = br r >>= return . Right

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
       else err $ show ("modDiv rev out of Range",a)

concChar :: ISO (Char,String) String
concChar = (fw,bw)
  where
    fw (c,r) = return (c:r)
    bw [] = err "concChar rew empty list"
    bw (c:r) = return (c,r)

modDivChar :: Word32 -> ISO Word32 Char
         -> ISO Word32 String -> ISO Word32 String
modDivChar n chrCoder rest
  = modDiv n <.> (pair chrCoder rest)  <.> concChar

reverseIso :: ISO String String
reverseIso = (return . reverse, return . reverse)

ifte :: ((a -> Bool), ISO a b) -> (ISO a c) -> ISO a (Either c b)
ifte  (cond,t) e = mkIso fw bw
  where
    fw x = if cond x
              then fwd t x >>= return . Right
              else fwd e x >>= return . Left
    bw (Left x) = rev e x
    bw (Right x) = rev t x

injectCase0 :: x -> x
injectCase0 = id
injectCase1 :: a -> Either a b
injectCase1 = Left
injectCase2 :: a -> Either (Either a b1) b2
injectCase2 = Left . injectCase1
injectCase3 :: a -> Either (Either (Either a b1) b2) b3
injectCase3 = Left . injectCase2
injectCase4 :: a -> Either (Either (Either (Either a b1) b2) b3) b4
injectCase4 = Left . injectCase3
injectCase5 :: a -> Either (Either (Either (Either (Either a b1) b2) b3) b4) b5
injectCase5 = Left . injectCase4
injectCase6
  :: a -> Either (Either (Either (Either (Either (Either a b1) b2) b3) b4) b5) b6
injectCase6 = Left . injectCase5

data Switch a i b where
  Case :: (a -> Bool , ISO a c , c -> b) -> Switch a i b
          -> Switch a (Either i c)  b
  Else :: (ISO a c , c -> b) -> Switch a c b

switchForward :: Switch a i b -> a -> ES b
switchForward sw x = case sw of
  Else (iso, inj) -> fwd iso x >>= return . inj
  Case (cond , iso , inj) rest
    -> if cond x
          then fwd iso x >>= return . inj
          else switchForward rest x

switchReverse :: Switch a i b -> i -> ES a
switchReverse sw x = case sw of
  Else (iso, _ ) -> rev iso x
  Case (cond , iso , _ ) rest -> case x of
    Left x' -> switchReverse rest x'
    Right v -> do
      a <- rev iso v
      if cond a then return a else err "rev switchReverse"

switch :: (b -> i) ->  Switch a i b -> ISO a b
switch f sw
  = mkIso (switchForward sw) (switchReverse sw . f)

casePoint
  :: Eq a => a -> b -> Switch a i b -> Switch a (Either i ()) b
casePoint a p
  = Case ((==) a  , point a () , \() -> p) 
