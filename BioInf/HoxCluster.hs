
-- | Run all steps of the HoxCluster algorithms in order.
--
-- This will produce the following:
-- 
-- 1. run the minimal distance algorithm, give the minimal distance score
-- and return all co-optimal paths
--
-- 2. run the end-probability algorithm and return the probability that
-- each node is the begin/end of a chain
--
-- 3. run the edge probability algorithm and give the probability for each
-- @from :-> to@ edge
--
-- 4. with the edge probabilities, run the maximal probability path
-- algorithm, return that probability and all co-optimal paths
--
-- TODO -Pretty should yield a structure to be given to the eps or svg
-- generator. This allows more flexibility. Does diagrams offer
-- serialization?
--
-- TODO All this should be wrapped and available as a function. not just
-- providing output files.

module BioInf.HoxCluster
  ( runHoxCluster
  , FillWeight (..)
  , FillStyle (..)
  ) where

import           Control.Monad (forM_)
import           Data.Function (on)
import           Data.List (groupBy)
import           Numeric.Log
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.FilePath (addExtension)
import           System.IO (withFile,IOMode(WriteMode))
import           Text.Printf

import           Data.PrimitiveArray (fromEdgeBoundaryFst,(:.)(..))
import           Data.PrimitiveArray.ScoreMatrix
import           Diagrams.TwoD.ProbabilityGrid
import           ShortestPath.SHP.Edge.MinDist (runMaxEdgeProb, runCoOptDist, boundaryPartFun,PathBT(..))
import           ADP.Fusion.Term.Edge.Type (From(..),To(..))

import           BioInf.HoxCluster.EdgeProb (edgeProbScoreMatrix, edgeProbPartFun)
--import           BioInf.HoxCluster.MinDist (runMaxEdgeProb, runCoOptDist, boundaryPartFun)
--import           BioInf.HoxCluster.ScoreMat



runHoxCluster
  :: FillWeight
  -> FillStyle
  -> Double
  -- ^ "Temperature" for probability-related parts of the algorithms.
  -- Lower temperatures favor a single path.
  -> FilePath
  -- ^ The input score matrix
  -> String
  -- ^ In the current directory, create output files with this name prefix
  -> IO ()
runHoxCluster fw fs temperature inFile filePrefix = do
  scoreMat <- fromFile inFile
  let lon = listOfRowNames scoreMat
  let n = length lon
  let lns = map T.unpack lon
  let bcols = max 4 . maximum $ map T.length $ lon
  withFile (filePrefix `addExtension` ".run") WriteMode $ \hrun -> do
    hPrintf hrun ("Input File: %s\n") inFile
    hPrintf hrun ("Temperature: %f\n") temperature
    hPrintf hrun ("\n")
    let (minD, minDcoopts) = runCoOptDist scoreMat
    --
    -- Print the minimal distance and the co-optimal paths
    --
    hPrintf hrun "Minimal Distance: %6.3f\n" minD
    hPrintf hrun "Optimal Paths:\n"
    forM_ minDcoopts (T.hPutStrLn hrun)
    hPrintf hrun "\n"
    --
    -- end probabilities, both to the output file and create pretty file
    --
    hPrintf hrun "Chain Begin/End Probabilities:\n"
    let bps = boundaryPartFun temperature scoreMat
    forM_ lon $ hPrintf hrun ("%" ++ show (bcols + 4) ++ "s")
    hPrintf hrun "\n"
    forM_ bps $ \(_, Exp p) -> hPrintf hrun ("%" ++ show (bcols + 4) ++ ".4f") (exp p)
    hPrintf hrun "\n"
    hPrintf hrun "\n"
    svgGridFile (filePrefix `addExtension` "boundary.svg") fw fs 1 n [] lns (Prelude.map snd bps)
    --
    -- edge probabilities, output file and pretty file
    --
    hPrintf hrun "Edge Probabilities:\n"
    let eps = edgeProbPartFun temperature scoreMat
    hPrintf hrun ("%" ++ show (bcols + 4) ++ "s") ("" :: String)
    forM_ lon $ hPrintf hrun ("%" ++ show (bcols + 4) ++ "s")
    hPrintf hrun "\n"
    forM_ (groupBy ((==) `on` (fromEdgeBoundaryFst . fst)) eps) $ \rps -> do
      let (eb,_) = head rps
      hPrintf hrun ("%" ++ show (bcols + 4) ++ "s") (lon !! fromEdgeBoundaryFst eb)
      forM_ rps $ \(eb,Exp p) -> hPrintf hrun ("%" ++ show (bcols + 4) ++ ".4f") (exp p)
      hPrintf hrun "\n"
    svgGridFile (filePrefix `addExtension` "edge.svg") fw fs n n lns lns (Prelude.map snd eps)
    --
    -- maximum probability path
    --
    hPrintf hrun "\n"
    let probMat = edgeProbScoreMatrix scoreMat eps
    let (Exp maxP, maxPcoopts) = runMaxEdgeProb probMat
    hPrintf hrun "Maximal Log-Probability Path Score: %6.3f\n" maxP
    forM_ maxPcoopts $ \path -> do
      forM_ path $ \case
        BTnode (_:.To n)    -> hPrintf hrun "%s\n" (lns !! n)
        BTedge (From ff:._) -> hPrintf hrun "%s -> " (lns !! ff)
      hPrintf hrun "\n"
    hPrintf hrun "\n"

