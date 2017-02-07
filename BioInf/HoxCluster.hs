
module BioInf.HoxCluster where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Numeric.Log
import           Data.List (nub,sort)
import           Control.Monad (forM_)
import           Text.Printf

import           ADP.Fusion.Set1
import           ADP.Fusion.Core
import           ADP.Fusion.Unit
import           Data.PrimitiveArray hiding (toList)
import           FormalLanguage

import BioInf.HoxCluster.ScoreMat



-- A small grammar for Hamiltonian path problems. We need three rules due
-- to normalization requirements for Inside-Outside.
--
-- Now, not a single node is set.
--
-- @
-- X -> mpty <<< e
-- @
--
-- A single node may be inserted, if the remainder of the set is then
-- empty. This means that the @s@ terminal checks that only sets of size
-- one are looked at.
--
-- @
-- X -> node <<< s X
-- @
--
-- An edge @k@ can be inserted, if at least one element in the set is still
-- empty, and the set already contains at least one element.
--
-- @
-- X -> edge <<< k X
-- @
--
-- TODO generalize to be SHP and move into shortest path problem library

[formalLanguage|
Verbose
Grammar: Hox
N: X
N: Y
T: s
T: k
S: Y
X -> mpty <<< ε     -- empty set
X -> node <<< s     -- single node
X -> edge <<< X k   -- edge k
Y -> fini <<< X     -- extract just the co-optimal ones
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
  , fini = id
  , h    = SM.foldl' max (-999999)
  }
{-# Inline aMinDist #-}

-- | This should give the correct order of nodes independent of the
-- underlying @Set1 First@ or @Set1 Last@ because the @(From:.To)@ system
-- is agnostic over these.
--
-- TODO Use text builder

aPretty :: Monad m => ScoreMat t -> SigHox m Text [Text] (From:.To) Int
aPretty s = SigHox
  { edge = \x (From f:.To t) -> T.concat [s `nameOf` f, "->", s `nameOf` t , "   " , x]
  , mpty = \()  -> ""
  , node = \n   -> s `nameOf` n -- ok because it is the first node in the path
  , fini = id
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
  , fini = id
  , h    = SM.foldl' (+) 0
  }
{-# Inline aInside #-}



type TS1 x = TwITbl Id Unboxed EmptyOk (BS1 First I)      x
type U   x = TwITbl Id Unboxed EmptyOk (Unit I)           x
type PF  x = TwITbl Id Unboxed EmptyOk (Boundary First I) x

type BT1 x b = TwITblBt Unboxed EmptyOk (BS1 First I) x Id Id b
type BTU x b = TwITblBt Unboxed EmptyOk (Unit I)      x Id Id b

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

-- | Extract the individual partition scores.

partFun :: ScoreMat Double -> Z:.TS1 (Log Double):.PF (Log Double)
partFun scoreMat =
  let n       = numNodes scoreMat
      partMat = toPartMatrix 100 scoreMat
  in  mutateTablesST $ gHox (aInside partMat)
        (ITbl 0 0 EmptyOk (fromAssocs (BS1 0 (-1)) (BS1 (2^n-1) (Boundary $ n-1)) (-999999) []))
        (ITbl 1 0 EmptyOk (fromAssocs (Boundary 0) (Boundary $ n-1)               (-999999) []))
        Edge
        Singleton
{-# NoInline partFun #-}

{-

-- | Fill table for maximal scoring paths.

runMaxScore :: ()
runMaxScore = undefined
{-# NoInline runMaxScore #-}



-- | Attach backtracking for maximal scoring paths.

runPretty :: ()
runPretty = undefined
{-# NoInline runPretty #-}



-- | Inside scoring.

runInside :: ()
runInside = undefined
{-# NoInline runInside #-}



-- | Outside scoring.

runOutside :: ()
runOutside = undefined
{-# NoInline runOutside #-}



-- | Combine Inside and Outside to get the probability that a particular
-- edge is taken.

runEdgeProbabilities :: ()
runEdgeProbabilities = undefined
{-# NoInline runEdgeProbabilities #-}



-- | Combine Inside and Outside to get the probability that a particular
-- start node is being used.

runStartProbabilities :: ()
runStartProbabilities = undefined
{-# NoInline runStartProbabilities #-}



-- | Combine Inside and Outside to get the probability that a particular
-- end node is being used.

runEndProbabilities :: ()
runEndProbabilities = undefined
{-# NoInline runEndProbabilities #-}

-}


test fp = do
  sMat <- fromFile fp
  {-
  let Z:.md1:.fin = forwardMinDist1 sMat
  let TW (ITbl _ _ _ itbl1) _ = md1
  let TW (ITbl _ _ _ itblu) _ = fin
  print itblu
  -}
  let (d,bt) = runCoOptDist sMat
  let Z:._:.TW (ITbl _ _ _ pf) _ = partFun sMat
  let ps' = assocs pf
  let pssum = Numeric.Log.sum $ Prelude.map snd ps'
  let ps = Prelude.map (\(Boundary b,p) -> (b,p / pssum)) ps'
  print d
  mapM_ print $ bt
  print $ length bt
  print $ length $ nub $ sort bt
  forM_ ps $ \(b,Exp p) -> printf "%s: %0.4f  " (sMat `nameOf` b) (exp p)
  putStrLn ""

