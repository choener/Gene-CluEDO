
module BioInf.HoxCluster where

import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Numeric.Log

import           ADP.Fusion.Core
import           Data.PrimitiveArray hiding (toList)
import           FormalLanguage



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
T: s
T: k
S: X
X -> mpty <<< ε     -- empty set
X -> node <<< s X   -- single node
X -> edge <<< k X   -- edge k
//
Outside: Xoh
Source: Hox
//
Emit: Hox
Emit: Xoh
|]



-- | maximal score algebra.
--
-- TODO The two Ints are the indices of the nodes and could be replaced?

aMaxScore :: Monad m => SigHox m Double Double Int Int
aMaxScore = SigHox
  { hEdge = \edge x -> error "hEdge" + x
  , hMpty = \() -> 0
  , hNode = \node x -> error "hNode" + x
  , hH    = SM.foldl' max (-999999)
  }
{-# Inline aMaxScore #-}

aPretty :: Monad m => SigHox m String [String] Int Int
aPretty = SigHox
  { hEdge = \edge x -> show edge ++ "," ++ x
  , hMpty = \()     -> ""
  , hNode = \node x -> show node ++ x -- ok because it is the first node in the path
  , hH    = SM.toList
  }
{-# Inline aPretty #-}

aInside :: Monad m => SigHox m (Log Double) (Log Double) Int Int
aInside = SigHox
  { hEdge = \edge x -> error "hEdge" * x
  , hMpty = \() -> 1
  , hNode = \node x -> error "hNode" * x
  , hH    = SM.foldl' (+) 0
  }
{-# Inline aInside #-}



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

