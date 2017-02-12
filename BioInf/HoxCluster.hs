
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

module BioInf.HoxCluster where

import           Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Printf
import           System.IO (withFile,IOMode(WriteMode))
import           System.FilePath (addExtension)
import           Numeric.Log
import           Data.List (groupBy)
import           Data.Function (on)

import           Data.PrimitiveArray (fromEdgeBoundaryFst)
import           Diagrams.TwoD.ProbabilityGrid

import           BioInf.HoxCluster.EdgeProb (edgeProbScoreMat, edgeProbPartFun)
import           BioInf.HoxCluster.MinDist (runMaxEdgeProb, runCoOptDist, boundaryPartFun)
import           BioInf.HoxCluster.ScoreMat

runHoxCluster
  :: Double
  -- ^ "Temperature" for probability-related parts of the algorithms.
  -- Lower temperatures favor a single path.
  -> FilePath
  -- ^ The input score matrix
  -> String
  -- ^ In the current directory, create output files with this name prefix
  -> IO ()
runHoxCluster temperature inFile filePrefix = do
  scoreMat <- fromFile inFile
  let lon = listOfNames scoreMat
  let n = length lon
  let lns = map T.unpack lon
  let bcols = maximum $ map T.length $ lon
  withFile (filePrefix `addExtension` ".run") WriteMode $ \hrun -> do
    let (minD, minDcoopts) = runCoOptDist scoreMat
    --
    -- Print the minimal distance and the co-optimal paths
    --
    hPrintf hrun "Minimal Distance: %6.3f\n" minD
    forM_ minDcoopts (T.hPutStrLn hrun)
    --
    -- end probabilities, both to the output file and create pretty file
    --
    let bps = boundaryPartFun temperature scoreMat
    forM_ lon $ hPrintf hrun ("%" ++ show (bcols + 2) ++ "s")
    hPrintf hrun "\n"
    svgGridFile (filePrefix `addExtension` "-boundary.svg") FWfill FSopaLin 1 n [] lns (Prelude.map snd bps)
    --
    -- edge probabilities, output file and pretty file
    --
    let eps = edgeProbPartFun temperature scoreMat
    forM_ lon $ hPrintf hrun ("%" ++ show (bcols + 2) ++ "s")
    forM_ (groupBy ((==) `on` (fromEdgeBoundaryFst . fst)) eps) $ \rps -> do
      let (eb,_) = head rps
      hPrintf hrun ("%" ++ show (bcols + 2) ++ "s") (lon !! fromEdgeBoundaryFst eb)
      forM_ rps $ \(eb,Exp p) -> hPrintf hrun ("%" ++ show (bcols + 2) ++ ".4f") (exp p)
    svgGridFile (filePrefix `addExtension` "-edge.svg") FWfill FSopaLin n n lns lns (Prelude.map snd eps)
    --
    -- maximum probability path
    --
    let probMat = edgeProbScoreMat scoreMat eps
    let (Exp maxP, maxPcoopts) = runMaxEdgeProb probMat
    hPrintf hrun "Maximal Log-Probability Path Score: %6.3f\n" maxP
    forM_ maxPcoopts (T.hPutStrLn hrun)

