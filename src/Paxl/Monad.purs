module Paxl.Monad
  ( GenPaxl(..)
  , Env
  , Val
  , Result(..)
  , Cont(..)
  , BlockedFetch(..)
  , ResultVal(..)
  , toPaxl
  ) where

import Prelude

import Control.Monad.Aff (Aff, Error, ParAff, parallel, sequential)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff.Ref (Ref)
import Data.List (List)
import Paxl.Effect (type (:+), GenPaxlEffects)
import Unsafe.Coerce (unsafeCoerce)


newtype GenPaxl req eff a =
  GenPaxl (Env req eff → Aff (GenPaxlEffects req :+ eff) (Result req eff a))


type Env req eff =
  { fetcher ∷ forall a. Array (BlockedFetch req a) → ParAff (GenPaxlEffects req :+ eff) Unit
  , pending ∷ Ref (List (BlockedFetch req Val))
  }


data Val


data Result req eff a
  = Done a
  | Throw Error
  | Blocked (Cont req eff a)


data Cont req eff a
  = Cont (GenPaxl req eff a)
  | Bind (Cont req eff Val) (Val → GenPaxl req eff a)
  | Fmap (Val → a) (Cont req eff Val)


makeBind ∷ ∀ req eff a b. Cont req eff a → (a → GenPaxl req eff b) → Cont req eff b
makeBind m k = Bind (unsafeCoerce m) (unsafeCoerce k)

infixl 1 makeBind as :>>=


makeFmap ∷ ∀ req eff a b. (a → b) → Cont req eff a → Cont req eff b
makeFmap f m = Fmap (unsafeCoerce f) (unsafeCoerce m)

infixl 4 makeFmap as :<$>


makeFlap ∷ ∀ req eff a b. Cont req eff (a → b) → a → Cont req eff b
makeFlap f m = (\f' → f' m) :<$> f

infixl 4 makeFlap as :<@>


toPaxl ∷ ∀ req eff a. Cont req eff a → GenPaxl req eff a
toPaxl (Cont paxl) = paxl
toPaxl (Bind m k) =
  case m of
    Cont paxl → paxl >>= k
    Bind m' k' → toPaxl (m' :>>= (k' >=> k))
    Fmap f x → toPaxl (x :>>= (f >>> k))
toPaxl (Fmap f x) =
  case x of
    Cont paxl → f <$> paxl
    Bind m k → toPaxl (m :>>= (k >>> map f))
    Fmap f' x' → toPaxl ((f <<< f') :<$> x')


instance functorGenPaxl ∷ Functor (GenPaxl req eff) where
  map f (GenPaxl m) = GenPaxl \env →
    m env <#> case _ of
      Done a → Done (f a)
      Throw e → Throw e
      Blocked cont → Blocked (f :<$> cont)

instance applyGenPaxl ∷ Apply (GenPaxl req eff) where
  apply (GenPaxl ff) (GenPaxl fa) = GenPaxl \env →
      sequential (go <$> parallel (ff env) <*> parallel (fa env))
    where
      go (Throw e) _ = Throw e
      go _ (Throw e) = Throw e
      go (Done f) (Done a) = Done (f a)
      go (Done f) (Blocked acont) =
        Blocked (f :<$> acont)
      go (Blocked fcont) (Done a) =
        Blocked (fcont :<@> a)
      go (Blocked fcont) (Blocked acont) =
        Blocked (Cont (toPaxl fcont <*> toPaxl acont))

instance applicativeGenPaxl ∷ Applicative (GenPaxl req eff) where
  pure a = GenPaxl \_ → pure (Done a)

instance bindGenPaxl ∷ Bind (GenPaxl req eff) where
  bind (GenPaxl fm) k = GenPaxl \env → do
    m ← fm env
    case m of
      Done a → let GenPaxl m = k a in m env
      Throw e → pure (Throw e)
      Blocked cont → pure (Blocked (cont :>>= k))

instance monadGenPaxl ∷ Monad (GenPaxl req eff)


newtype BlockedFetch req a = BlockedFetch
  { request ∷ req a
  , blockedVar ∷ AVar (ResultVal a)
  }


data ResultVal a
  = Ok a
  | ThrowPaxl Error
