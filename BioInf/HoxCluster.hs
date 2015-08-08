
module BioInf.HoxCluster where

import ADP.Fusion
import Data.PrimitiveArray hiding (toList)
import FormalLanguage



[formalLanguage|
Verbose
Grammar: Hox
N: X
T: s
T: b
T: i
S: X
X -> mpty <<<     X e
X -> node <<<     X s
X -> edge <<< b i X
//
|]

