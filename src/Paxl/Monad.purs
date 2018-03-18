module Paxl.Monad
  ( GenPaxl(..)
  , Env
  , Val
  , Result(..)
  , Cont(..)
  , toPaxl
  ) where

import Prelude

import Control.Monad.Aff (Aff, Error, ParAff, parallel, sequential)
import Control.Monad.Eff.Ref (Ref)
import Data.List (List)
import Paxl.Effect (GenPaxlEffects)
import Paxl.RequestStore (BlockedFetch, RequestStore)
import Unsafe.Coerce (unsafeCoerce)


newtype GenPaxl req a =
  GenPaxl (Env req → Aff (GenPaxlEffects req ()) (Result req a))


type Env req =
  { fetcher ∷ forall a. Array (BlockedFetch req a) → ParAff (GenPaxlEffects req ()) Unit
  , pending ∷ Ref (List (BlockedFetch req Val))
  , requestStore ∷ RequestStore req Val
  }


data Val


data Result req a
  = Done a
  | Throw Error
  | Blocked (Cont req a)


data Cont req a
  = Cont (GenPaxl req a)
  | Bind (Cont req Val) (Val → GenPaxl req a)
  | Fmap (Val → a) (Cont req Val)


makeBind ∷ ∀ req a b. Cont req a → (a → GenPaxl req b) → Cont req b
makeBind m k = Bind (unsafeCoerce m) (unsafeCoerce k)

infixl 1 makeBind as :>>=


makeFmap ∷ ∀ req a b. (a → b) → Cont req a → Cont req b
makeFmap f m = Fmap (unsafeCoerce f) (unsafeCoerce m)

infixl 4 makeFmap as :<$>


makeFlap ∷ ∀ req a b. Cont req (a → b) → a → Cont req b
makeFlap f m = (\f' → f' m) :<$> f

infixl 4 makeFlap as :<@>


toPaxl ∷ ∀ req a. Cont req a → GenPaxl req a
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


instance functorGenPaxl ∷ Functor (GenPaxl req) where
  map f (GenPaxl m) = GenPaxl \env →
    m env <#> case _ of
      Done a → Done (f a)
      Throw e → Throw e
      Blocked cont → Blocked (f :<$> cont)

instance applyGenPaxl ∷ Apply (GenPaxl req) where
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

instance applicativeGenPaxl ∷ Applicative (GenPaxl req) where
  pure a = GenPaxl \_ → pure (Done a)

instance bindGenPaxl ∷ Bind (GenPaxl req) where
  bind (GenPaxl fm) k = GenPaxl \env → do
    m ← fm env
    case m of
      Done a → let GenPaxl m = k a in m env
      Throw e → pure (Throw e)
      Blocked cont → pure (Blocked (cont :>>= k))

instance monadGenPaxl ∷ Monad (GenPaxl req)
