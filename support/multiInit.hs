-----------------------------------------------------------------------------
-- |
-- Module      :  
-- Copyright   :  (c) Robert Lee (2010)
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  robert.lee@chicago.vc
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Creates first generation of Genotype and Phenotype
--
-----------------------------------------------------------------------------

import Genome

import Data.Binary
import qualified Data.ByteString.Lazy as L
import System.IO
import Data.List
import Control.Monad

import Text.Template
import qualified Data.Map as Map

import Data.List.Split (splitEvery)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M

gen :: L.ByteString -> (a -> b) -> [a] -> [b]
gen bRnds gene names = map (\x -> pool !! chooser (length pool) x) rnds
  where
    pool = map gene names
    rnds = getRnds bRnds
    chooser = flip mod

fork1s :: L.ByteString -> [Fork]
fork1s bRnds = map (\g -> Fork1 g dummy) $ gen bRnds Gene1
               [
                "invM"
               ,"invA"
               ,"succX"
               ,"notX"
--               ,"isEven"
--               ,"sinX"
--               ,"cosX"
               ]

fork2s :: L.ByteString -> [Fork]
fork2s bRnds = map (\g -> Fork2 g dummy dummy) $ gen bRnds Gene2
               [
                "plus"
               ,"mult"
               ,"lt"
               ,"gt"
--               ,"eq"
               ,"orX"
               ,"andX"
--               ,"eqB"
--               ,"downIf"
               ,"compose"
               ]

fork3s :: L.ByteString -> [Fork]
fork3s bRnds = map (\g -> Fork3 g dummy dummy dummy) $ gen bRnds Gene3
               ["ifX"]

forks :: L.ByteString -> [Fork] -> [Fork] -> [Fork] -> [Fork]
forks rRnds f1sΓ f2sΓ f3sΓ = alt rnds f1sΓ f2sΓ f3sΓ
  where
    alt :: [Rnd] -> [Fork] -> [Fork] -> [Fork] -> [Fork]
    alt (r:rs') f1s@(f1:f1s') f2s@(f2:f2s') f3s@(f3:f3s')
        | choice > 70 = f1 : alt rs' f1s' f2s  f3s
        | choice >= 0 = f2 : alt rs' f1s  f2s' f3s
        | otherwise   = f3 : alt rs' f1s  f2s  f3s'
      where
        choice = r `mod` 100

    rnds = getRnds rRnds

leafs :: L.ByteString -> L.ByteString -> L.ByteString -> [Leaf]
leafs rRnds dRnds bRnds = map (Leaf . Terminal) $ alt rnds doubles bools
  where
    alt :: [Rnd] -> [Double] -> [Bool] -> [String]
    alt (r:rs') ds@(d:ds') bs@(b:bs')
        | choice <= 3 = enc ("D (" ++ show d ++ ")") : alt rs' ds' bs
--        | choice == 1 = enc ("B " ++ show b) : alt rs' ds  bs'
        | otherwise = "term Reflector"     : alt rs' ds  bs
      where
        choice = r `mod` 10

    rnds = getRnds rRnds
    unsigned = map (\d -> (fromIntegral d / fromIntegral (maxBound::Int))) rnds
    doubles = map ((+ (-5)) . (10*)) unsigned
--    doubles = map ((5 -) . fromIntegral) . map (`mod` 10) $ rnds
    bools = getBools bRnds
    enc cs = "term(" ++ cs ++ ")"

growCs :: Int -> L.ByteString -> [(Int -> Bool)]
growCs maxD rRnds = map cmp rnds
  where
    rnds = getRnds rRnds
    cmp r d = r `mod` maxD == 0 || d >= maxD

fullCs :: Int -> [(Int -> Bool)]
fullCs maxD = cycle [(>= maxD)] 

type GTuple = ([(Int -> Bool)], [Fork], [Leaf], Genome)

-- individual is like zip
-- root is always a fork
individual :: [(Int -> Bool)] -> [Fork] -> [Leaf] -> GTuple
individual csΓ fsΓ lsΓ = fork 1 csΓ fsΓ lsΓ 
  where
    leafOrFork d (c:cs') fs ls@(l:ls')
      | c d = leaf
      | otherwise = fork (d + 1) cs' fs ls
      where
        leaf = (cs', fs, ls', l)

    fork d cs (Fork1 g _:fs) ls = (csΔ, fsΔ, lsΔ, Fork1 g m)
      where
        (csΔ, fsΔ, lsΔ, m) = leafOrFork d cs fs ls
    fork d cs (Fork2 g _ _:fs) ls = (csΔ', fsΔ', lsΔ', Fork2 g l r)
      where
        (csΔ,  fsΔ,  lsΔ,  l) = leafOrFork d cs fs ls
        (csΔ', fsΔ', lsΔ', r) = leafOrFork d csΔ fsΔ lsΔ
    fork d cs (Fork3 g _ _ _:fs) ls = (csΔ'', fsΔ'', lsΔ'', Fork3 g l m r)
      where
        (csΔ,   fsΔ,   lsΔ,   l) = leafOrFork d cs fs ls
        (csΔ',  fsΔ',  lsΔ',  m) = leafOrFork d csΔ fsΔ lsΔ
        (csΔ'', fsΔ'', lsΔ'', r) = leafOrFork d csΔ' fsΔ' lsΔ'

population :: [(Int -> Bool)] -> [Fork] -> [Leaf] -> [GTuple]
population csΓ fsΓ lsΓ = init $ foldr step [(csΓ, fsΓ, lsΓ, dummy)] [1..popSize]
  where
    step _ prev@((csΔ, fsΔ, lsΔ, _):_) = individual csΔ fsΔ lsΔ:prev


popSize = 5000

main = do
  f1LBS <- extRands
  f2LBS <- extRands
  f3LBS <- extRands
  lrLBS <- extRands
  ldLBS <- extRands
  lbLBS <- extRands
  frLBS <- extRands
  grLBS <- extRands

  let f1s = fork1s f1LBS
      f2s = fork2s f2LBS
      f3s = fork3s f3LBS
      fs  = forks frLBS f1s f2s f3s
      lfs = leafs lrLBS ldLBS lbLBS

      gCs = growCs maxD grLBS
      fCs = fullCs maxD

      growGTuples = population gCs fs lfs
      fullGTuples = (\(_, fsΔ, lsΔ, _) -> population fCs fsΔ lsΔ)
                    $ head growGTuples

      pop = map (\(_, _, _, g) -> g) $ growGTuples ++ fullGTuples




      nPhenoHs = map (nPheno2hs) . zip [0..] $ map display pop
      nPheno2hs (n, p) = (name, declaration ++ definition)
        where
          declaration = name ++ " :: (X a) => a -> a\n"
          definition = name ++ " = " ++ p ++ "\n\n"
          name = "pI" ++ show n

      phenoParts :: [(Int, [((String, String), Genome)])]
      phenoParts = zip [0..] . splitEvery splitSize $ zip nPhenoHs pop

      phenOut :: (Int, [((String, String), Genome)]) -> IO ()
      phenOut (serial,ps) = do
        popTemplate <- readTemplate templateName
        renderToFile fileName popTemplate $ context tmplVars
        where
          fileName = "Population" ++ show serial ++ ".hs"
          tmplVars = [("phenotypes", concatMap (snd . fst) ps)
                     ,("genotypes", outString $ map (show . snd) ps)
                     ,("serial", show serial)
                     ,("exports", exports)
                     ]
          exports = intercalate ", " $ map (fst . fst) ps


  forM_ phenoParts phenOut


  let
      summTmplVars = [("popImports", imports)
                     ,("population", intercalate ", " $ map fst nPhenoHs)
                     ,("gpops", intercalate ", " $ map (("gpop" ++) . show . fst) phenoParts)
                     ,("generation", "0")
                     ]

      imports = intercalate "\n"
                . map ("import " ++)
                . map (("Population" ++) . show . fst) $ phenoParts
   
  succTemplate <- readTemplate summaryTempName
  renderToFile summaryFileName succTemplate $ context summTmplVars

  
 

  where
    extRands = openFile "/dev/urandom" ReadMode >>= L.hGetContents
    maxD = 5
    splitSize = 200
    summaryTempName = "summaryTemplate.hs"
    summaryFileName = "Summary.hs"
    templateName = "genXMultiTemp.hs"

