module Paxl.Prelude
  ( type (+)
  , type (:+)
  , ApplyRowType
  , ApplyRowEffect
  , (<|)
  , (|>)
  , (<<)
  , (>>)
  , discard
  , module ReExports
  ) where

import Control.Apply (class Apply, (*>))
import Control.Monad.Eff (kind Effect)
import Control.Semigroupoid (compose, composeFlipped)
import Data.Function (apply, applyFlipped)
import Data.Unit (Unit, unit)
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, disj, div, eq, flap, flip, gcd, id, ifM, join, lcm, liftA1, liftM1, map, max, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as ReExports



type ApplyRowType (f ∷ # Type → # Type) r = f r
infixr 0 type ApplyRowType as +

type ApplyRowEffect (f ∷ # Effect → # Effect) r = f r
infixr 0 type ApplyRowEffect as :+

-- personal stylistic preferences
infixr 0 apply as <|
infixl 1 applyFlipped as |>
infixr 9 compose as >>
infixl 9 composeFlipped as <<

-- this gives us parallelism when executing a series of commands like
-- do exp1
--    exp2
--    exp3
-- in order to get sequential behaviour, one must explicitly denote
-- do _ ← exp1
--    _ ← exp2
--    exp3
discard ∷ ∀ f a. (Apply f) ⇒ f Unit → (Unit → f a) → f a
discard f1 f2 = f1 *> f2 unit
