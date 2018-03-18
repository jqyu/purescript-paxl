module Paxl
  ( PAXL
  , Paxl
  , PaxlEffects
  , ApplyRowType
  , type (+)
  , module ReExports
  , (<|)
  , (|>)
  ) where

import Control.Monad.Eff (kind Effect)
import Data.Function (apply, applyFlipped)
import Paxl.Effect (GENPAXL, GenPaxlEffects)
import Paxl.Effect (type (:+)) as ReExports
import Paxl.Fetch (Fetch)
import Paxl.Monad (GenPaxl)
import Paxl.Run (initEnv, runPaxl) as ReExports
import Paxl.Parallel (Par(..), Seq(..)) as ReExports


type ApplyRowType (f ∷ # Type → # Type) r = f r
infixr 0 type ApplyRowType as +


type Paxl reqs = GenPaxl (Fetch reqs)

type PAXL reqs = GENPAXL (Fetch reqs)

type PaxlEffects reqs eff = GenPaxlEffects (Fetch reqs) eff

-- personal stylistic preference
infixr 0 apply as <|
infixl 1 applyFlipped as |>
