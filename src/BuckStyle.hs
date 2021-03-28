{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, FlexibleInstances #-}
{-# INLINE filter #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  BuckStyle
-- Copyright   :  (c) Robert Lee (2010)
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  robert.lee@chicago.vc
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Phenotype Library
--
-----------------------------------------------------------------------------


module BuckStyle (
                  Scalar(..), Terminal(..), X,
                  ifX, term, compose, succX, invA, invM, eqB, sinX, cosX,
                  isEven, notX, mult, plus, gt, lt, eq, andX, orX, downIf
                 )
where

-- Phenotype --

data Terminal = D Double | B Bool | Reflector
-- Terminals mask a single type within a scalar

data D1 = D1 [Scalar] Scalar
data D2 = D2 [D1] Scalar
data Scalar = Scalar Double Bool deriving (Show, Eq)

class V a
instance V [Scalar]
instance V [D1]

-- class X guarantees type sanity in the evolving population

class X a where
  oneS :: a -> (Scalar -> Scalar) -> a
  twoS :: a -> a -> (Scalar -> Scalar -> Scalar) -> a
  threeS :: a -> a -> a -> (Scalar -> Scalar -> Scalar -> Scalar) -> a
  ifS :: a -> a -> a -> a
  comp :: (a -> a) -> (a -> a) -> a -> a
  oneV :: forall b . X b =>
          a -> (b -> b) -> (forall v . V v => ((b -> b) -> v -> v)) -> a

  comp l r = r . l

instance X D1 where
  oneS (D1 v s) f = D1 v (f s)
  twoS (D1 _ sl) (D1 v sr) f = (D1 v (f sl sr))
  threeS (D1 _ sl) (D1 _ sm) (D1 v sr) f = (D1 v (f sl sm sr))
  ifS (D1 _ (Scalar _ b)) = (b ?) 
  oneV (D1 v s) mf f = D1 (f mf v) s

instance X D2 where
  oneS (D2 v s) f = (D2 v (f s))
  twoS (D2 _ sl) (D2 v sr) f = (D2 v (f sl sr))
  threeS (D2 _ sl) (D2 _ sm) (D2 v sr) f = (D2 v (f sl sm sr))
  ifS (D2 _ (Scalar _ b)) = (b ?)
  oneV (D2 v s) mf f = D2 (f mf v) s

instance X Scalar where
  oneS s f = (f s)
  twoS sl sr f = (f sl sr)
  threeS sl sm sr f = (f sl sm sr)
  ifS (Scalar _ b) = (b ?)
  oneV s _ _ = s

type X1 = (X a) => (a -> a) -> a -> a
type X2 = (X a) => (a -> a) -> (a -> a) -> a -> a
type X3 = (X a) => (a -> a) -> (a -> a) -> (a -> a) -> a -> a



(?) :: Bool -> a -> a -> a
(?) b a = \c -> if b then a else c

compose = comp

term :: (X a) => Terminal -> a -> a
term (D d) x = oneS x op
  where op (Scalar _ b) = (Scalar d b)
term (B b) x = oneS x op
  where op (Scalar d _) = (Scalar d b)
term Reflector x = x

succX :: X1
succX f x = oneS (f x) op
  where op (Scalar d b) = Scalar (d + 1) b

invM :: X1
invM f x = oneS (f x) op
  where op (Scalar d b) = let result = 1.0 / d
                          in
                            if isInfinite result
                            then Scalar 1.0 b
                            else Scalar result b

invA :: X1
invA f x = oneS (f x) op
  where op (Scalar d b) = Scalar (d * (-1)) b

isEven :: X1
isEven f x = oneS (f x) op
  where op (Scalar d _) = Scalar d (even $ round d)

notX :: X1
notX f x = oneS (f x) op
  where op (Scalar d b) = Scalar d (not b)

sinX :: X1
sinX f x = oneS (f x) op
  where op (Scalar d b) = Scalar (sin d) b

cosX :: X1
cosX f x = oneS (f x) op
  where op (Scalar d b) = Scalar (cos d) b

plus :: X2
plus fl fr x = twoS (fl x) (fr x) op
  where op (Scalar d1 _) (Scalar d2 b) = Scalar (d1 + d2) b

mult :: X2
mult fl fr x = twoS (fl x) (fr x) op
  where op (Scalar d1 _) (Scalar d2 b) = Scalar (d1 * d2) b

gt :: X2
gt fl fr x = twoS (fl x) (fr x) op
  where op (Scalar d1 _) (Scalar d2 _) = Scalar d2 (d1 > d2)

lt :: X2
lt fl fr x = twoS (fl x) (fr x) op
  where op (Scalar d1 _) (Scalar d2 _) = Scalar d2 (d1 < d2)

eq :: X2
eq fl fr x = twoS (fl x) (fr x) op
  where op (Scalar d1 _) (Scalar d2 _) = Scalar d2 (d1 == d2)

eqB :: X2
eqB fl fr x = twoS (fl x) (fr x) op
  where op (Scalar _ b1) (Scalar d b2) = Scalar d (b1 == b2)

andX :: X2
andX fl fr x = twoS (fl x) (fr x) op
  where op (Scalar d1 b1) (Scalar d2 b2) = Scalar (b1 ? d2 $ d1) (b1 && b2)

orX :: X2
orX fl fr x = twoS (fl x) (fr x) op
  where op (Scalar d1 b1) (Scalar d2 b2) = Scalar (b1 ? d1 $ d2) (b1 || b2)

downIf :: X2
downIf fl fr x = ifS x (fl x) (fr x)

ifX :: X3
ifX fl fm fr x = ifS (fl x) (fm x) (fr x)


{-
  map
  foldl
  scanl
  chunk
  take
  drop
  transpose
  zipWith

-}
