module Paxl.Effect
  ( GENPAXL
  , GenPaxlEffects
  , GenPaxlST
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

data GenPaxlST
