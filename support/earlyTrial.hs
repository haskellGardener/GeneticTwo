import Data.Binary as B
import qualified Data.ByteString.Lazy as L
import System.IO as S
import Data.List (sortBy, transpose)
import Data.List.Split (splitEvery, splitPlaces)

import Data.Array

type Rnd = Int

data LGenome = Terminal Double
             | Variable Int
             | FIndex Int


instance Show LGenome where
  show (Terminal d) = "Terminal " ++ show d
  show (Variable n) = "Variable " ++ show n
  show (FIndex n)   = "Function " ++ (show . fst $ functionSet !! n)

maxVars :: Int
maxVars = 1

percentVars :: Int
percentVars = 10

popSize :: Int
popSize = 5

maxIndividual :: Int
maxIndividual = 20

minIndividual :: Int
minIndividual = 6


functionSet :: [(String, Double -> Double -> Double)]
functionSet = [("safe /", safeDivide),("*", (*)),("-", (-))]
--functionSet = [(+), (-), safeDivide, (*)]

safeDivide :: Double -> Double -> Double
safeDivide n d = if isInfinite result then n else result
  where result = n / d

terminals :: [Double]
terminals = [i * n | i <- [1..100], n <- [1, -1]]

randFunc :: Rnd -> LGenome
randFunc rnd = FIndex (rnd `mod` length functionSet)

randTerm :: Rnd -> LGenome
randTerm rnd = Terminal (terminals !! (rnd `mod` length terminals))

randVar :: Rnd -> LGenome
randVar rnd = Variable (rnd `mod` maxVars)

tuple2 :: [a] -> [(a,a)]
tuple2 = map tup2 . splitEvery 2
  where
    tup2 (x:y:[]) = (x,y)

tuple3 :: [a] -> [(a,a,a)]
tuple3 = map tup3 . splitEvery 3
  where
    tup3 (x:y:z:[]) = (x,y,z)

type LIndividual = [LGenome]


mkIndividual :: [Rnd] -> (LIndividual, [Rnd])
mkIndividual (rnd:rnds) =
  let
    indSize = (rnd `mod` (maxIndividual - minIndividual)) + minIndividual

    funcSize = indSize `div` 2
    varSize = ceiling (fromIntegral funcSize * (fromIntegral percentVars / 100))
    termSize = funcSize - varSize

    (fRnds:tRnds:vRnds:tvRTRnds:(rest:[])) =
      splitPlaces [funcSize,termSize,varSize,varSize + termSize,999999999] rnds

    funcs = map randFunc fRnds
    terms = map randTerm tRnds
    vars  = map randVar  vRnds

    tvList    = concat [terms, vars] -- Not randomized yet!
    tvRTuples = zip tvList $ take (varSize + termSize) tvRTRnds
    tvRList   = map fst $ sortBy sndOrd tvRTuples -- This randomizes things

    sndOrd (_, r) (_, r') | r > r' = GT
                          | otherwise = LT
  in
    (concat . transpose $ [funcs, tvRList], rest)

mkPopulace :: L.ByteString -> [LIndividual]
mkPopulace bRnds = map fst $ foldr step [([],rnds)] [1..popSize]
  where
    rnds = map B.decode $ bnSplit 8 bRnds

    step _ [([], rest)] = [mkIndividual rest]
    step _ xs@(x:_)     = (mkIndividual $ snd x) : xs

bnSplit :: Int -> L.ByteString -> [L.ByteString]
bnSplit n xs = (L.take (fromIntegral n) xs) : (bnSplit n $ L.drop (fromIntegral n) xs)

main =
    do
      handle <- openFile "/dev/urandom" ReadMode
      stream <- L.hGetContents handle
--      S.putStrLn . show . bs2Int $ stream
--      stream' <- L.hGetContents handle
--      S.putStrLn . show . bs2Int $ stream'
--      S.putStrLn . show . bs2Int $ L.drop 4 stream
      let populace = mkPopulace stream
      putStrLn . show . length $ populace
      putStrLn . show $ populace
--      putStrLn . show . tuple2 . head $ populace

      let fPopulace = map mkFuncInd populace

      putStrLn . show $ [ind i | ind <- fPopulace, i <- [0,33..5000]]

--      putStrLn . show $ (mkFuncInd (head . tail $ populace)) 0
--      let aInd = loadIndividual $ head populace
--      putStrLn . show $ aInd

      hClose handle
--      putStrLn . concatMap (++ "\n") . map show . mkPopulace $ stream

{-
type Individual = Array Int (LGenome, LGenome)

loadIndividual :: LIndividual -> Individual
loadIndividual listIndividual =
    listArray (0,aSize) t2
    where
      t2 = tuple2 listIndividual
      aSize = length t2 - 1
-}

variable :: Int -> Double
variable 0 = 42.5

mkFuncInd :: LIndividual -> (Double -> Double)
mkFuncInd = foldr step (+0) . tuple2
  where
    step :: (LGenome, LGenome) -> (Double -> Double) -> (Double -> Double)
--        step (FIndex i, Terminal t) gain = (snd $ (functionSet !! i)) t . gain
--        step (FIndex i, Variable v) gain = (snd $ (functionSet !! i)) (variable v) . gain
    step (FIndex i, Terminal t) gain = gain . (snd $ (functionSet !! i)) t
    step (FIndex i, Variable v) gain = gain . (snd $ (functionSet !! i)) (variable v)

{-
--        path     scion  
findCP :: [Rnd] -> Genome -> ([Rnd], [(Rnd, Genome)])
findCP rnds candidate = (drop (length find0) rnds,
                              zip rnds (find' rnds candidate))
  where
    find0 = find' rnds candidate

    find' :: [Rnd] -> Genome -> [Genome]
    find' (r:rs) f@(Fork1 _ gm) = f:find' rs gm
    find' (r:rs) f@(Fork2 _ gl gr) | r `mod` 2 == 0 = f:find' rs gl
                                   | otherwise = f:find' rs gr
    find' (r:rs) f@(Fork3 _ gl gm gr) | choose == 0 = f:find' rs gl
                                      | choose == 1 = f:find' rs gm
                                      | choose == 2 = f:find' rs gr
      where choose = r `mod` 3
    find' (r:rs) l@(Leaf _) = [l]

-}


