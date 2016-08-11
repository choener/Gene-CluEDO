
module BioInf.HoxCluster where

import ADP.Fusion.Core
import Data.PrimitiveArray hiding (toList)
import FormalLanguage



[formalLanguage|
Verbose
Grammar: Hox
N: X
T: s
T: k
S: X
X -> mpty <<< e     -- empty set
X -> node <<< s     -- single node
X -> edge <<< k X   -- edge k
//
|]

