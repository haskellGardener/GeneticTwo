module Summary (generation, gpop, population)
where

import BuckStyle
import Genome

${popImports}

generation :: Int
generation = ${generation}

population :: X a => [(a -> a)]
population = [${population}]

gpop :: [Genome]
gpop = concat [${gpops}]

