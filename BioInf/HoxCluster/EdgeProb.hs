
module BioInf.HoxCluster.EdgeProb where

import           Control.Arrow (second)
import           Control.Monad (forM_)
import           Data.List (nub,sort)
import           Data.Text (Text,unpack)
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

import           BioInf.HoxCluster.ScoreMat



[formalLanguage|
Verbose
Grammar: Hox
N: F
N: L
N: Z
T: s
T: k
S: Z
F -> mpty <<< ε       -- empty set
F -> node <<< s       -- single node
F -> edge <<< F k     -- edge k
L -> mpty <<< ε       -- empty set
L -> node <<< s       -- single node
L -> edge <<< L k     -- edge k
Z -> fini <<< L k F   -- edges bracketed by a Last and a First set
//
Emit: Hox
|]

makeAlgebraProduct ''SigHox

-- | Minimal distance algebra
--
-- TODO The two Ints are the indices of the nodes and could be replaced?

aMinDist :: Monad m => ScoreMat Double -> SigHox m Double Double (From:.To) Int
aMinDist s = SigHox
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

aPretty :: Monad m => ScoreMat t -> SigHox m Text [Text] (From:.To) Int
aPretty s = SigHox
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

aInside :: Monad m => ScoreMat (Log Double) -> SigHox m (Log Double) (Log Double) (From:.To) Int
aInside s = SigHox
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
--type PF  x = TwITbl Id Unboxed EmptyOk (Boundary First I) x

type BF1 x b = TwITblBt Unboxed EmptyOk (BS1 First I)    x Id Id b
type BL1 x b = TwITblBt Unboxed EmptyOk (BS1 Last  I)    x Id Id b
type BEB x b = TwITblBt Unboxed EmptyOk (EdgeBoundary I) x Id Id b

{-
-- | Run the minimal distance algebra.
--
-- This produces one-boundary sets. Meaning that for each boundary we get
-- the total distance within the set.

forwardMinDist1 :: ScoreMat Double -> Z:.TS1 Double:.U Double
forwardMinDist1 scoreMat =
  let n = numNodes scoreMat
  in  mutateTablesST $ gHox (aMinDist scoreMat)
        (ITbl 0 0 EmptyOk (fromAssocs (BS1 0 (-1)) (BS1 (2^n-1) (Boundary $ n-1)) (-999999) []))
        (ITbl 1 0 EmptyOk (fromAssocs Unit         Unit                           (-999999) []))
        Edge
        Singleton
{-# NoInline forwardMinDist1 #-}

backtrackMinDist1 :: ScoreMat Double -> Z:.TS1 Double:.U Double -> [Text]
backtrackMinDist1 scoreMat (Z:.ts1:.u) = unId $ axiom b
  where !(Z:.bt1:.b) = gHox (aMinDist scoreMat <|| aPretty scoreMat)
                            (toBacktrack ts1 (undefined :: Id a -> Id a))
                            (toBacktrack u   (undefined :: Id a -> Id a))
                            Edge
                            Singleton
                        :: Z:.BT1 Double Text:.BTU Double Text
{-# NoInline backtrackMinDist1 #-}

-- | Given the @Set1@ produced in @forwardMinDist1@ we can now extract the
-- co-optimal paths using the @Set1 -> ()@ index change.
--
-- TODO do we want this one explicitly or make life easy and just extract
-- from all @forwardMinDist1@ paths?

runCoOptDist :: ScoreMat Double -> (Double,[Text])
runCoOptDist scoreMat = (unId $ axiom fwdu,bs)
  where !(Z:.fwd1:.fwdu) = forwardMinDist1 scoreMat
        bs = backtrackMinDist1 scoreMat (Z:.fwd1:.fwdu)
{-# NoInline runCoOptDist #-}
-}

-- | Extract the individual partition scores.

partFun :: Double -> ScoreMat Double -> [(EdgeBoundary I, Log Double)]
partFun temperature scoreMat =
  let n       = numNodes scoreMat
      partMat = toPartMatrix temperature scoreMat
      (Z:.sF:.sL:.sZ) = mutateTablesST $ gHox (aInside partMat)
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
{-# NoInline partFun #-}

test t fp = do
  sMat <- fromFile fp
  let n = numNodes sMat
  let lns = fmap unpack $ listOfNames sMat
--  let (d,bt) = runCoOptDist sMat
  let ps = partFun t sMat
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
  svgGridFile "test.svg" FWlinear n n lns lns (Prelude.map snd ps)

