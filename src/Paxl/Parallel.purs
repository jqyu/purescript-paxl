-- Rather than returning `Unit` for effectful requests,
-- returning `Par` signifies that a request can be executed in parallel
-- with any following computations (e.g. for debugging purposes)
-- while returning `Seq` signifies that any later requests should wait
-- on the current request (e.g. for mutations)

module Paxl.Parallel
  ( Par(..)
  , Seq(..)
  ) where


import Prelude
import Data.Monoid (class Monoid)

data Par = Par
data Seq = Seq

instance discardPar ∷ Discard Par where
  discard p1 p2 = p1 *> p2 Par

instance discardSeq ∷ Discard Seq where
  discard = bind


-- misc instances

instance semigroupPar ∷ Semigroup Par where
  append _ _ = Par

instance semigroupSeq ∷ Semigroup Seq where
  append _ _ = Seq

instance monoidPar ∷ Monoid Par where
  mempty = Par

instance monoidSeq ∷ Monoid Seq where
  mempty = Seq
