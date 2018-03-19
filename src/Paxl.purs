module Paxl
  ( PAXL
  , Paxl
  , PaxlEffects
  , module ReExports
  ) where

import Paxl.Effect (GENPAXL, GenPaxlEffects)
import Paxl.Fetch (Fetch)
import Paxl.Monad (GenPaxl)

-- re-exports
import Paxl.Run (initEnv, runPaxl) as ReExports
-- import Paxl.Prelude as ReExports
import Paxl.Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (+), type (:+), type (~>), ApplyRowEffect, ApplyRowType, Ordering(EQ, GT, LT), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, id, ifM, join, lcm, liftA1, liftM1, map, max, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<), (<<<), (<=), (<=<), (<>), (<@>), (<|), (=<<), (==), (>), (>=), (>=>), (>>), (>>=), (>>>), (|>), (||), (∘), (≪), (≫), (▷), (◁), (⬦)) as ReExports


type Paxl reqs = GenPaxl (Fetch reqs)

type PAXL reqs = GENPAXL (Fetch reqs)

type PaxlEffects reqs eff = GenPaxlEffects (Fetch reqs) eff
