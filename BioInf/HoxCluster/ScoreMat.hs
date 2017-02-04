
-- | Import score matrices. These are type-isomorphic to Blast matrices.
--
-- TODO Generalize @Lib-BiobaseBlast@ to support importing matrices of
-- these types.

module BioInf.HoxCluster.ScoreMat where

import           Data.Text (Text)
import           Data.Vector.Unboxed (Unbox)
import qualified Data.Vector as V

import           ADP.Fusion.Term.Edge.Type (From(..), To(..))
import           Data.PrimitiveArray hiding (map)


-- | NxN sized score matrices
--
-- TODO needs a vector with the column names!

data ScoreMat t = ScoreMat (Unboxed (Z:.Int:.Int) t) (V.Vector Text)

-- | Get the distance between edges @From -> To@.

(.!.) :: Unbox t => ScoreMat t -> From:.To -> t
ScoreMat mat _ .!. (From f:.To t) = mat ! (Z:.f:.t)
{-# Inline (.!.) #-}

-- | Get the name of the node at an index

nameOf :: ScoreMat t -> Int -> Text
nameOf (ScoreMat _ ns) k = ns V.! k
{-# Inline nameOf #-}

-- | Number of nodes in a score matrix.

numNodes :: Unbox t => ScoreMat t -> Int
numNodes (ScoreMat mat _) = let ((Z:.0:.0),(Z:.n':._)) = bounds mat in n' -1
{-# Inline numNodes #-}

