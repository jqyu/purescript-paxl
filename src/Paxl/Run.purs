module Paxl.Run (initEnv, runPaxl) where

import Paxl.Prelude

import Control.Monad.Aff (ParAff, sequential, throwError)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Array (fromFoldable) as Array
import Data.List (List(..), null) as List
import Paxl.Effect (GenPaxlEffects)
import Paxl.Fetch (class Fetchable, BlockedFetch, fetch)
import Paxl.Monad (GenPaxl(..), Env, Result(..), toPaxl)
import Paxl.RequestStore (new) as RequestStore
import Unsafe.Coerce (unsafeCoerce)


initEnv ∷ ∀ req env reqEff eff m. MonadEff ( ref ∷ REF | eff ) m ⇒ Fetchable req env reqEff ⇒ env → m (Env req)
initEnv userEnv = liftEff do
  { fetcher: unsafeLiftToPaxlEffects (fetch userEnv)
  , pending: _
  , requestStore: _
  } <$> newRef List.Nil
    <*> (unsafeCoerceEff RequestStore.new)


type Fetcher req eff = ∀ a. Array (BlockedFetch req a) → ParAff eff Unit

unsafeLiftToPaxlEffects ∷ ∀ req eff1. (Fetcher req eff1) → (Fetcher req (GenPaxlEffects req ()))
unsafeLiftToPaxlEffects = unsafeCoerce


runPaxl ∷ ∀ req eff a m. MonadAff ( GenPaxlEffects req :+ eff ) m ⇒ Env req → GenPaxl req a → m a
runPaxl env@{ fetcher, pending } =
    liftAff ∘ unsafeCoerceAff ∘ go
  where
    go (GenPaxl m) = do
      result ← m env
      case result of
        Done a → pure a
        Throw err → throwError err
        Blocked cont → unsafeCoerceAff do
          enqueuedFetches ← liftEff do
            readRef pending <* writeRef pending List.Nil
          when (not (List.null enqueuedFetches)) do
            sequential (fetcher (Array.fromFoldable enqueuedFetches))
          go ◁ toPaxl cont
