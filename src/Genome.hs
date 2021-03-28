
-----------------------------------------------------------------------------
-- |
-- Module      :  Genome
-- Copyright   :  (c) Robert Lee (2010)
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  robert.lee@chicago.vc
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Genome Library
--
-----------------------------------------------------------------------------

module Genome (Rnd, Genome(..), Terminal(..), Gene1(..), Gene2(..), Gene3(..)
              ,Fork, Leaf, Interval(..), dummy
              ,display, bnSplit, getRnds, getBools, (?), outString, context
              )
where

import Data.List (intercalate)
import qualified Data.ByteString.Lazy as L
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M


type Rnd = Int

data Genome = Leaf Terminal
            | Fork1 Gene1 Genome
            | Fork2 Gene2 Genome Genome
            | Fork3 Gene3 Genome Genome Genome
              deriving (Show, Read)

data Terminal = Terminal String deriving (Show, Read)
data Gene1 = Gene1 String deriving (Show, Read)
data Gene2 = Gene2 String deriving (Show, Read)
data Gene3 = Gene3 String deriving (Show, Read)

type Fork = Genome
type Leaf = Genome

dummy = Leaf $ Terminal ""

data Interval = I Double Double deriving (Show)

-- Admittedly these instances are VERY weird and perverted, but they work.

instance Eq Interval
  where
    (==) (I l u) (I l' u') = al <= bl && au > bu
      where ((I al au), (I bl bu)) =
                if l == l'
                then if u == u'
                     then ((I l u), (I l' u'))
                     else if u > u'
                          then ((I l u), (I l' u'))
                          else ((I l' u'), (I l u))
                else if l < l'
                     then ((I l u), (I l' u'))
                     else ((I l' u'), (I l u))

instance Ord Interval
  where
    compare i@(I l u) i'@(I l' u') = if i == i'
                                     then EQ
                                     else if l < l'
                                          then LT
                                          else GT

infixr 5 |||

(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs

(?) :: Bool -> a -> a -> a
(?) b a = \c -> if b then a else c

bnSplit :: Int -> L.ByteString -> [L.ByteString]
bnSplit n xs = (L.take (fromIntegral n) xs) : (bnSplit n $ L.drop (fromIntegral n) xs)

getRnds :: L.ByteString -> [Rnd]
getRnds lbs = map (abs . decode) $ bnSplit 8 lbs

getBools :: L.ByteString -> [Bool]
getBools lbs = map even (map decode $ bnSplit 1 lbs :: [Word8])


display :: Genome -> String
display (Leaf (Terminal s)) = concat ["(", s, ")"]
display (Fork1 (Gene1 s) gm) = concat ["(", s, display gm, ")"]
display (Fork2 (Gene2 s) gl gr) = concat ["(", s, display gl, display gr, ")"]
display (Fork3 (Gene3 s) gl gm gr) = concat ["(", s
                                            ,display gl, display gm, display gr
                                            ,")"
                                            ]
 
outString :: [String] -> String
outString = intercalate ",\n" . map ("          " ++)

{- context is swiped from the Text.Template hackage page -} 
context = M.fromList . map packPair
    where packPair (x, y) = (B.pack x, B.pack y)

