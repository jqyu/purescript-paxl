module Paxl.Effect
  ( GENPAXL
  , GenPaxlEffects
  , ApplyRowEffect
  , GenPaxlST
  , type (:+)
  ) where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.ST (ST)


foreign import data GENPAXL ∷ (Type -> Type) -> Effect

type GenPaxlEffects req eff =
  ( avar ∷ AVAR
  , paxl ∷ GENPAXL req
  , ref ∷ REF
  , st ∷ ST GenPaxlST
  | eff
  )

type ApplyRowEffect (f ∷ # Effect → # Effect) r = f r
infixr 0 type ApplyRowEffect as :+

data GenPaxlST
