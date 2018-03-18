module Paxl.Prelude
  ( type (+)
  , type (:+)
  , ApplyRowType
  , ApplyRowEffect
  , (<|)
  , (|>)
  , discard
  , module ReExports
  ) where

import Control.Apply (class Apply, (*>))
import Control.Monad.Eff (kind Effect)
import Control.Semigroupoid (compose, composeFlipped)
import Data.Function (apply, applyFlipped)
import Data.Unit (Unit, unit)
import Prelude hiding (($), (#), discard) as ReExports


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
