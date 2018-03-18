module Paxl.RequestStore
  ( Result(..)
  , BlockedFetch(..)
  , RequestStore
  , Key(..)
  , CacheResult(..)
  , class Cacheable
  , cacheKey
  , new
  , peek
  , poke
  , delete
  , prefixKey
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVar, tryReadVar)
import Control.Monad.Eff.Exception (Error)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap.ST (STStrMap)
import Data.StrMap.ST (delete, new, peek, poke) as STStrMap
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Paxl.Effect (GenPaxlEffects, GenPaxlST)


data Result a
  = Ok a
  | Throw Error

derive instance functorResult ∷ Functor Result


newtype BlockedFetch req a = BlockedFetch
  { request ∷ req a
  , blockedVar ∷ AVar (Result a)
  }

derive instance blockedFetchNewtype ∷ Newtype (BlockedFetch req a) _


newtype RequestStore req a = RequestStore (STStrMap GenPaxlST (BlockedFetch req a))

newtype Key = Key String

data CacheResult a
  = Resolved (Result a)
  | Waiting (AVar (Result a))
  | Uncached

class Cacheable req where
  cacheKey ∷ ∀ a. req a → Key


new ∷ ∀ req eff a. Eff (GenPaxlEffects req eff) (RequestStore req a)
new = RequestStore <$> STStrMap.new


peek ∷ ∀ req eff a. RequestStore req a → Key → Eff (GenPaxlEffects req eff) (CacheResult a)
peek (RequestStore map) (Key key) = do
  entry ← STStrMap.peek map key
  case entry of
    Nothing → pure Uncached
    Just (BlockedFetch { blockedVar }) → do
      tryReadVar blockedVar <#> case _ of
        Nothing → Waiting blockedVar
        Just result → Resolved result


poke ∷ ∀ req eff a. RequestStore req a → Key → BlockedFetch req a → Eff (GenPaxlEffects req eff) Unit
poke (RequestStore map) (Key key) bf = void do
  STStrMap.poke map key bf


delete ∷ ∀ req eff a. RequestStore req a → Key → Eff (GenPaxlEffects req eff) Unit
delete (RequestStore map) (Key key) = void do
  STStrMap.delete map key


prefixKey ∷ ∀ sym. IsSymbol sym ⇒ SProxy sym → Key → Key
prefixKey proxy (Key k) = Key (reflectSymbol proxy <> "∷" <> k)
