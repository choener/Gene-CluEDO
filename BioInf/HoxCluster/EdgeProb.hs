
module BioInf.HoxCluster.EdgeProb where

import           Control.Arrow (second)
import           Control.Monad (forM_)
import           Data.List (nub,sort)
import           Data.Text (Text,unpack)
import           Data.Vector.Unboxed (Unbox)
import           Numeric.Log
import qualified Data.Text as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Text.Printf

import           ADP.Fusion.Core
import           ADP.Fusion.EdgeBoundary
import           ADP.Fusion.Set1
import           Data.PrimitiveArray hiding (toList)
import           Diagrams.TwoD.ProbabilityGrid
import           FormalLanguage
import           ShortestPath.SHP.EdgeProb

import           BioInf.HoxCluster.ScoreMat



-- | Minimal distance algebra
--
-- TODO The two Ints are the indices of the nodes and could be replaced?

aMinDist :: Monad m => ScoreMat Double -> SigEdgeProb m Double Double (From:.To) Int
aMinDist s = SigEdgeProb
  { edge = \x e -> x + (s .!. e)
  , mpty = \() -> 0
  , node = \n -> 0
  , fini = \l e f -> l + (s .!. e) + f
  , h    = SM.foldl' min 999999
  }
{-# Inline aMinDist #-}

-- | This should give the correct order of nodes independent of the
-- underlying @Set1 First@ or @Set1 Last@ because the @(From:.To)@ system
-- is agnostic over these.
--
-- TODO Use text builder

aPretty :: Monad m => ScoreMat t -> SigEdgeProb m Text [Text] (From:.To) Int
aPretty s = SigEdgeProb
  { edge = \x (From f:.To t) -> T.concat [s `nameOf` f, " -> ", x]
  , mpty = \()  -> ""
  , node = \n   -> s `nameOf` n -- ok because it is the first node in the path
  , fini = \l (From i:.To j) f -> T.concat [l, "~>~", f]
  , h    = SM.toList
  }
{-# Inline aPretty #-}

-- | Before using @aInside@ the @ScoreMat@ needs to be scaled
-- appropriately! Due to performance reasons we don't want to do this
-- within @aInside@.

aInside :: Monad m => ScoreMat (Log Double) -> SigEdgeProb m (Log Double) (Log Double) (From:.To) Int
aInside s = SigEdgeProb
  { edge = \x e -> s .!. e * x
  , mpty = \() -> 1
  , node = \n -> 1
  , fini = \l e f -> l * (s .!. e) * f
  , h    = SM.foldl' (+) 0
  }
{-# Inline aInside #-}



type TF1 x = TwITbl Id Unboxed EmptyOk (BS1 First I)      x
type TL1 x = TwITbl Id Unboxed EmptyOk (BS1 Last  I)      x
type EB  x = TwITbl Id Unboxed EmptyOk (EdgeBoundary I)   x

type BF1 x b = TwITblBt Unboxed EmptyOk (BS1 First I)    x Id Id b
type BL1 x b = TwITblBt Unboxed EmptyOk (BS1 Last  I)    x Id Id b
type BEB x b = TwITblBt Unboxed EmptyOk (EdgeBoundary I) x Id Id b



-- | Extract the individual partition scores.

edgeProbPartFun :: Double -> ScoreMat Double -> [(EdgeBoundary I, Log Double)]
edgeProbPartFun temperature scoreMat =
  let n       = numNodes scoreMat
      partMat = toPartMatrix temperature scoreMat
      (Z:.sF:.sL:.sZ) = mutateTablesST $ gEdgeProb (aInside partMat)
                          (ITbl 0 0 EmptyOk (fromAssocs (BS1 0 (-1)) (BS1 (2^n-1) (Boundary $ n-1)) 0 []))
                          (ITbl 1 0 EmptyOk (fromAssocs (BS1 0 (-1)) (BS1 (2^n-1) (Boundary $ n-1)) 0 []))
                          (ITbl 2 0 EmptyOk (fromAssocs (0 :-> 0)    (0 :-> (n-1))                  0 []))
                          Edge
                          Singleton
                        :: Z:.TF1 (Log Double):.TL1 (Log Double):.EB (Log Double)
      TW (ITbl _ _ _ pf) _ = sZ
      bs' = assocs pf
      pssum = (Numeric.Log.sum $ Prelude.map snd bs') / (fromIntegral n - 1)
      bs = Prelude.map (second (/pssum)) bs'
  in bs
{-# NoInline edgeProbPartFun #-}

-- | Turn the edge probabilities into a score matrix.

edgeProbScoreMat :: (Unbox t) => ScoreMat t -> [(EdgeBoundary I, Log Double)] -> ScoreMat (Log Double)
edgeProbScoreMat (ScoreMat mat ns) xs' = ScoreMat m ns
  where m = fromAssocs l h 0 xs
        (l,h) = bounds mat
        xs = [ ((Z:.f:.t),p) | (f :-> t, p) <- xs' ]

{-
test t fp = do
  sMat <- fromFile fp
  let n = numNodes sMat
  let lns = fmap unpack $ listOfNames sMat
--  let (d,bt) = runCoOptDist sMat
  let ps = edgeProbPartFun t sMat
--  print d
--  mapM_ print $ bt
--  print $ length bt
--  print $ length $ nub $ sort bt
  forM_ ps $ \(b :-> _,_) -> printf "%5s  " (sMat `nameOf` b)
  putStrLn ""
  forM_ ps $ \(_ :-> b,_) -> printf "%5s  " (sMat `nameOf` b)
  putStrLn ""
  forM_ ps $ \(_,Exp p) -> printf "%0.3f  " (exp p)
  putStrLn ""
  let Exp z = Numeric.Log.sum $ Prelude.map snd ps
  printf "sum: %0.3f\n" $ exp z
  forM_ ps $ \(_,Exp p) -> printf "%0.3f  " (1 / (1 - p))
  putStrLn ""
  putStrLn ""
  putStrLn ""
  print n
  print lns
  print $ length ps
  print ps
  svgGridFile "test.svg" FWfill FSopaLin n n lns lns (Prelude.map snd ps)
-}

