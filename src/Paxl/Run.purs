module Paxl.Run (initEnv, runPaxl) where

import Prelude

import Control.Monad.Aff (Aff, ParAff, sequential, throwError)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Data.Array (fromFoldable) as Array
import Data.List (List(..), null) as List
import Paxl.Effect (type (:+), GenPaxlEffects)
import Paxl.Fetch (class Fetchable, fetch)
import Paxl.Monad (GenPaxl(..), Env, Result(..), BlockedFetch, toPaxl)
import Unsafe.Coerce (unsafeCoerce)


initEnv ∷ ∀ req env reqEff eff. Fetchable req env reqEff ⇒ env → Eff ( ref ∷ REF | eff ) (Env req)
initEnv userEnv =
  { fetcher: unsafeLiftToPaxlEffects (fetch userEnv)
  , pending: _
  } <$> newRef List.Nil


type Fetcher req eff = ∀ a. Array (BlockedFetch req a) → ParAff eff Unit

unsafeLiftToPaxlEffects ∷ ∀ req eff1. (Fetcher req eff1) → (Fetcher req (GenPaxlEffects req ()))
unsafeLiftToPaxlEffects = unsafeCoerce


runPaxl ∷ ∀ req eff a. Env req → GenPaxl req a → Aff ( GenPaxlEffects req :+ eff ) a
runPaxl env@{ fetcher, pending } (GenPaxl m) = unsafeCoerceAff do
  result ← m env
  case result of
    Done a → pure a
    Throw err → throwError err
    Blocked cont → unsafeCoerceAff do
      enqueuedFetches ← liftEff do
        readRef pending <* writeRef pending List.Nil
      when (not (List.null enqueuedFetches)) do
        sequential (fetcher (Array.fromFoldable enqueuedFetches))
      runPaxl env (toPaxl cont)
