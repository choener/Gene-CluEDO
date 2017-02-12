
-- | Import score matrices. These are type-isomorphic to Blast matrices.
--
-- TODO Generalize @Lib-BiobaseBlast@ to support importing matrices of
-- these types.

module BioInf.HoxCluster.ScoreMat where

import           Control.Monad (when,unless)
import           Data.Text (Text)
import           Data.Vector.Unboxed (Unbox)
import           Numeric.Log
import qualified Data.Text as T
import qualified Data.Vector as V
import           System.Exit (exitFailure)

import           ADP.Fusion.Term.Edge.Type (From(..), To(..))
import           Data.PrimitiveArray hiding (map)
import qualified Data.PrimitiveArray as PA



-- | NxN sized score matrices
--
-- TODO needs a vector with the column names!

data ScoreMat t = ScoreMat (Unboxed (Z:.Int:.Int) t) (V.Vector Text)
  deriving (Eq,Show)

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
numNodes (ScoreMat mat _) = let ((Z:.0:.0),(Z:.n':._)) = bounds mat in n' + 1
{-# Inline numNodes #-}

listOfNames :: ScoreMat t -> [Text]
listOfNames (ScoreMat _ ns) = V.toList ns

-- | Turns a @ScoreMat@ for distances into one scaled by "temperature" for
-- Inside/Outside calculations. Each value is scaled by
-- @\k -> exp $ negate k / r * t@ where
-- r = (n-1) * d
-- d = mean of genetic distance
--
-- TODO Again, there is overlap and we should really have @newtype
-- Distance@ and friends.
--
-- TODO @newtype Temperature = Temperature Double@

toPartMatrix
  :: Double
  -- ^ temperature
  -> ScoreMat Double
  -> ScoreMat (Log Double)
toPartMatrix t scoreMat@(ScoreMat mat names) = ScoreMat p names
  where p = PA.map (\k -> Exp {- . log . exp -} $ negate k / (r * t)) mat
        n = numNodes scoreMat
        d = Prelude.sum [ mat ! (Z:.i:.j) | i <- [0..n-1], j <- [i+1..n-1] ] / fromIntegral (n*(n-1))
        r = fromIntegral (n-1) * d

-- | Import data.
--
-- TODO Should be generalized because @Lib-BiobaseBlast@ does almost the
-- same thing.

fromFile :: FilePath -> IO (ScoreMat Double)
fromFile fp = do
  ls <- lines <$> readFile fp
  when (null ls) $ do
    putStrLn $ fp ++ " is empty"
    exitFailure
  let mat' = map (map read . tail . words) $ tail ls
  let n = length mat'
  unless (all ((==n) . length) mat') $ do
    putStrLn $ fp ++ " is not a NxN matrix"
    print mat'
    exitFailure
  let mat = PA.fromAssocs (Z:.0:.0) (Z:.n-1:.n-1) 0
          $ concatMap (\(r,es) -> [ ((Z:.r:.c),e) | (c,e) <- zip [0..] es ])
          $ zip [0..] mat' -- rows
  let names = V.fromList . map T.pack . drop 1 . words $ head ls
  return $ ScoreMat mat names

