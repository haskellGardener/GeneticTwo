module Population (population, gpop, run)
where

import BuckStyle
import Genome

run :: Int
run = ${run}

population :: X a => a -> [a]
population test = map ($$ test) xs
  where
    xs :: (X a) => [(a -> a)]
    xs = [
${pop}
         ]


gpop :: [Genome]
gpop =   [
${gen}
         ]
