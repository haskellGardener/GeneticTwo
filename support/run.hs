import BuckStyle
import Population
import Genome

import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing)
import Data.List (transpose, sortBy, sort)
import System.IO
import qualified Data.ByteString.Lazy as L
import Control.Arrow
import Data.List.Split (splitEvery, splitPlaces)
import Text.Template

--import Control.Parallel.Strategies
--import Control.Parallel
-- parMap rdeepseq rseq 

evalX :: X a => (a -> a -> a) -> [(a, a)] -> [[a]]
evalX fit tsts = transpose $ map pass tsts
  where
    pass (testVal, target) = map (fit target) (population testVal)

-- cf. Koza 1992 (95-98)

rawFit :: Scalar -> Scalar -> Scalar
rawFit (Scalar target targetb) (Scalar resultD resultB) =
  Scalar (abs $ target - resultD) (targetb == resultB)

adjFits :: [[Scalar]] -> [Double]
adjFits = map (adjfit . sum . map getD)
  where
    getD (Scalar d _) = d
    adjfit s = 1 / (1 + s)

normFits :: [Double] -> [Double]
normFits adjs = map (/ total) adjs
  where
    total = sum adjs

eval2MapI :: [(Scalar, Scalar)] -> (Map.Map Interval (Genome, (Int, Int)), [Double])
eval2MapI tsts = (Map.fromList . zip intervI $ zip gpop' sized, adjs)
  where
    adjs = adjFits . evalX rawFit $ tsts
    fitness = normFits adjs

    sorted = sortBy sortF . zip gpop $ fitness 
    sortF (_,d1) (_,d2) = d1 `compare` d2

    gpop' = map fst sorted
    fitness' = map snd sorted

    sized = map fNls gpop'

    intervT = uncurry zip . (init &&& tail) . scanl (+) 0 $ fitness'
    intervI = map (uncurry I) intervT


cut :: Map.Map Interval (Genome, (Int, Int)) -> (Rnd, Rnd, Rnd)
     -> Maybe ((Genome, (Genome -> Genome)))
cut pop (dR, lfR, cpR) = case selection of
                       Just individual -> Just $ cutX individual
                       Nothing -> Nothing
  where
    d = fromIntegral dR / fromIntegral (maxBound::Int)
    cutX (g, (f, l)) = if lfR `mod` 10 == 0
                       then cutL (cpR `mod` l) g
                       else cutF (cpR `mod` f) g

    selection = Map.lookup (I d d) pop


type FBuild = Genome -> Genome -> Genome

--     point   parent    (excised, build continuation)
cutF :: Int -> Genome -> (Genome, (Genome -> Genome))
cutF p candidate = case findC (-1) candidate of
                     (Right t) -> t
                     Left _ -> error $ "ERROR" ++ (show candidate)
  where
    findC :: Int -> Genome -> Either Int (Genome, (Genome -> Genome))
    findC n (Leaf _) = Left n

    findC n f@(Fork1 _ gm) = 
                case findC n gm of
                  Left n1 -> if n1 + 1 == p
                             then Right (f, id) -- Cut @ this fork
                             else Left (n1 + 1)
                  Right (ex, bf) -> Right (ex, fBuild1 f . bf)

    findC n f@(Fork2 _ gl gr) = lT
      where
        lT    = case findC n gl of
                  Left n1 -> if n1 + 1 == p
                             then Right (f, id) -- Cut @ this fork
                             else rT (n1 + 1)
                  Right (ex, bf) -> Right (ex, fBuild2l f . bf)
        rT n2 = case findC n2 gr of
                  Left n3 -> Left n3
                  Right (ex, bf) -> Right (ex, fBuild2r f . bf)

    findC n f@(Fork3 _ gl gm gr) = lT
      where
        lT    = case findC n gl of
                  Left n1 -> if n1 + 1 == p
                             then Right (f, id) -- Cut @ this fork
                             else mT (n1 + 1)
                  Right (ex, bf) -> Right (ex, fBuild3l f . bf)
        mT n2 = case findC n2 gm of
                  Left n3 -> rT n3
                  Right (ex, bf) -> Right (ex, fBuild3m f . bf)
        rT n4 = case findC n4 gr of
                  Left n5 -> Left n5
                  Right (ex, bf) -> Right (ex, fBuild3r f . bf)


--     point   parent    (excised, build continuation)
cutL :: Int -> Genome -> (Genome, (Genome -> Genome))
cutL p candidate = (\(Right t) -> t) $ findC (-1) candidate
  where
    findC :: Int -> Genome -> Either Int (Genome, (Genome -> Genome))
    findC n l@(Leaf _) = if n + 1 == p
                         then Right (l, id) -- Cut @ this Leaf
                         else Left (n + 1)

    findC n f@(Fork1 _ gm) = 
                case findC n gm of
                  Left n1 -> Left n1
                  Right (ex, bf) -> Right (ex, fBuild1 f . bf)

    findC n f@(Fork2 _ gl gr) = lT
      where
        lT    = case findC n gl of
                  Left n1 -> rT n1
                  Right (ex, bf) -> Right (ex, fBuild2l f . bf)
        rT n2 = case findC n2 gr of
                  Left n3 -> Left n3
                  Right (ex, bf) -> Right (ex, fBuild2r f . bf)

    findC n f@(Fork3 _ gl gm gr) = lT
      where
        lT    = case findC n gl of
                  Left n1 -> mT n1
                  Right (ex, bf) -> Right (ex, fBuild3l f . bf)
        mT n2 = case findC n2 gm of
                  Left n3 -> rT n3
                  Right (ex, bf) -> Right (ex, fBuild3m f . bf)
        rT n4 = case findC n4 gr of
                  Left n5 -> Left n5
                  Right (ex, bf) -> Right (ex, fBuild3r f . bf)


fBuild1, fBuild2l, fBuild2r, fBuild3l, fBuild3m, fBuild3r :: FBuild
fBuild1 (Fork1 g _) new = Fork1 g new

fBuild2l (Fork2 g _ r) new = Fork2 g new r
fBuild2r (Fork2 g l _) new = Fork2 g l new

fBuild3l (Fork3 g _ m r) new = Fork3 g new m r
fBuild3m (Fork3 g l _ r) new = Fork3 g l new r
fBuild3r (Fork3 g l m _) new = Fork3 g l m new

fNls :: Genome -> (Int, Int)
fNls (Leaf _) = (0, 1)
fNls (Fork1 _ gm) = fNlsum [(1,0),fNls gm]
fNls (Fork2 _ gl gr) = fNlsum [(1,0),fNls gl,fNls gr]
fNls (Fork3 _ gl gm gr) = fNlsum [(1,0),fNls gl,fNls gm,fNls gr]

fNlsum :: [(Int, Int)] -> (Int, Int)
fNlsum ps = (sum $ map fst ps, sum $ map snd ps)

maxDepth :: Genome -> Int
maxDepth (Leaf _) = 1
maxDepth (Fork1 _ gm) = succ $ maxDepth gm
maxDepth (Fork2 _ gl gr) = succ . maximum $ map maxDepth [gl, gr]
maxDepth (Fork3 _ gl gm gr) = succ . maximum $ map maxDepth [gl, gm, gr]


main = do
  dBS  <- extRands
  lfBS <- extRands
  cpBS <- extRands

  let 
      reproTot = floor (fromIntegral (length gpop) * 0.1)

      dRs  = getRnds dBS
      lfRs = getRnds lfBS
      cpRs = getRnds cpBS

      rnds = zip3 dRs lfRs cpRs
      (gpop', adjs) = eval2MapI sample
      cuts = catMaybes $ map (cut gpop') rnds
      parents = map (head &&& last) $ splitEvery 2 cuts
      cross ((g,f), (g',f')) = [f g', f' g]

      rootTest (Leaf _) = False
      rootTest _ = True

      rawXovers = concatMap cross parents
      xovers = filter ((maxD >=) . maxDepth) $ filter rootTest rawXovers
      repros = take reproTot . reverse . map (fst . snd) . Map.toList $ gpop'
      newPop = take (length gpop) (repros ++ xovers)

      outStr = outString $ map display newPop

--  putStrLn . show $ repros

  let adjsS = sort adjs

  putStrLn $ "Generation #" ++ show run
  putStrLn $ show (last adjsS) ++ " " ++ show (adjsS !! (length adjsS `div` 2)) ++ " " ++ show (head adjsS)

--  putStrLn . show . filter (not . rootTest) $ newPop

--  print . show . map (const "N") . filter isNothing $ map (cut gpop') rnds
--  putStrLn . concatMap (("\n" ++) . take 50 . show) $ xovers

  popTemplate <- readTemplate templateName
  renderToFile fileName popTemplate $ context [("pop", outStr)
                                              ,("gen", outString $ map show newPop)
                                              ,("run", show (run + 1))
                                              ]

  where
    extRands = openFile "/dev/urandom" ReadMode >>= L.hGetContents
    fileName = "Population.hs"
    templateName = "genXTemplate.hs"
    maxD = 9



sample :: [(Scalar, Scalar)]
sample = [((Scalar x True), (Scalar (sin x) True))| x <- [0.0,0.1..(6.0 * pi)]]
