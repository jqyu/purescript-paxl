module Test.Paxl.Aff
  ( LiftAff
  , AffWrapper
  , succeed
  , fail
  ) where

import Paxl

import Control.Monad.Aff (Aff, error, parallel, try)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Paxl.Fetch (class Fetchable, BlockedFetch(..), Req, ResultVal(..), completeBlockedFetch, inject, request)


type LiftAff eff reqs = ( aff ∷ Req (AffWrapper eff) | reqs )


succeed ∷ ∀ reqs eff a. Aff eff a → Paxl ( LiftAff eff + reqs ) a
succeed aff = request <| inject _aff <| Succeed aff


fail ∷ ∀ reqs eff a. Paxl ( LiftAff eff + reqs ) a
fail = request <| inject _aff <| Fail


_aff = SProxy ∷ SProxy "aff"


data AffWrapper eff a
  = Succeed (Aff eff a)
  | Fail


instance fetchableAffWrapper ∷ Fetchable (AffWrapper eff) a eff where
  fetch _ blocked =
    for_ blocked \bf@(BlockedFetch { request }) →
      parallel case request of
        Succeed cont → do
          val ← try cont
          completeBlockedFetch bf case val of
            Right v → Ok v
            Left err → ThrowPaxl err
        Fail → do
          completeBlockedFetch bf (ThrowPaxl (error "AffWrapper:Fail"))
