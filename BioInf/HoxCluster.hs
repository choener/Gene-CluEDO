
module BioInf.HoxCluster where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Numeric.Log

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
  { edge = \x (From f:._) -> T.concat [s `nameOf` f , "," , x]
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



type TS1 x = TwITbl Id Unboxed EmptyOk (BS1 First I) x
type U   x = TwITbl Id Unboxed EmptyOk (Unit I)      x

-- | Run the minimal distance algebra.
--
-- This produces one-boundary sets. Meaning that for each boundary we get
-- the total distance within the set.

forwardMinDist1 :: ScoreMat Double -> Z:.TS1 Double:.U Double
forwardMinDist1 scoreMat =
  let n = numNodes scoreMat
  in  mutateTablesDefault $ gHox (aMinDist scoreMat)
        (ITbl 0 0 EmptyOk (fromAssocs (BS1 0 (-1)) (BS1 (2^n-1) (Boundary $ n-1)) (-999999) []))
        (ITbl 1 0 EmptyOk (fromAssocs Unit         Unit                           (-999999) []))
        Edge
        Singleton
{-# NoInline forwardMinDist1 #-}

-- | Given the @Set1@ produced in @forwardMinDist1@ we can now extract the
-- co-optimal paths using the @Set1 -> ()@ index change.
--
-- TODO do we want this one explicitly or make life easy and just extract
-- from all @forwardMinDist1@ paths?

runCoOptDist :: Z:.TS1 Double -> (Double,[[String]])
runCoOptDist (Z:.ts1) = undefined
{-# NoInline runCoOptDist #-}

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
  let Z:.md1:.fin = forwardMinDist1 sMat
  let TW (ITbl _ _ _ itbl) _ = md1
  print itbl

