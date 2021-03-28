module Population${serial} (gpop${serial}, ${exports})
where

import BuckStyle
import Genome

${phenotypes}

gpop${serial} :: [Genome]
gpop${serial} = [
${genotypes}
       ]

