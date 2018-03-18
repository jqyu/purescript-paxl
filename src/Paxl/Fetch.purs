module Paxl.Fetch
  ( class Hashable
  , class Fetchable
  , class FetchableRows
  , Fetch
  , Req
  , getFetcher
  , fetch
  , hash
  , inject
  , request
  , completeBlockedFetch
  , completeBlockedFetchOf
  , module ReExports
  ) where

import Prelude

import Control.Monad.Aff (Aff, ParAff, error, parallel)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, readVar) as AVar
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (modifyRef)
import Data.Array (foldMap) as Array
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Leibniz (type (~))
import Data.List (List(..)) as List
import Data.Maybe (Maybe(..))
import Data.Record (delete, get, insert) as Record
import Data.StrMap (StrMap)
import Data.StrMap (lookup, singleton) as StrMap
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Paxl.Effect (type (:+), GenPaxlEffects)
import Paxl.Monad (GenPaxl(..), Env, Result(..), Cont(..), BlockedFetch(..), ResultVal(..))
import Type.Prelude (class RowLacks)
import Type.Row (class RowToList, RLProxy(..), Nil, Cons)
import Unsafe.Coerce (unsafeCoerce)

import Paxl.Monad (BlockedFetch(..), ResultVal(..)) as ReExports


class Hashable req where
  hash ∷ forall a. req a → String

class Fetchable req env eff | req → env, req env → eff where
  fetch ∷ env → forall a. Array (BlockedFetch req a) → ParAff eff Unit


-- for fetchable requests R1 a ⇒ E1, R2 a ⇒ E2, …, Rn a ⇒ En, we wish to define a type
--   Fetch ( r1 ∷ Req R1, …, rn ∷ Req Rn ) a ⇒ { r1 ∷ E1, …, rn ∷ En }
-- describing a variant over the given request types

data Fetch (fetches ∷ # Type) (a ∷ Type)

data Req (req ∷ Type → Type)

data FVal a

newtype FetchRep req a = FetchRep { sym ∷ String, req ∷ req a }

newtype PerformFetch req eff = PerformFetch forall a. Array (BlockedFetch req a) → ParAff eff Unit

-- ( foo ∷ Req R1, bar ∷ Req R2, … )
--   ⇒ { foo ∷ E1, bar ∷ R2, … } → { foo ∷ PerformFetch R1 eff, bar ∷ PerformFetch R2 eff, … }

class FetchableRows flist (frow ∷ # Type) erow prow (eff ∷ # Effect)
  | flist → frow, flist → erow, flist → prow, flist → eff where
  getFetcher ∷ RLProxy flist → Record erow → Record prow

instance fetchableRowsNil ∷ FetchableRows Nil () () () eff where
  getFetcher _ _ = {}

instance fetchableRowsCons ∷
  ( Fetchable req env eff
  , FetchableRows fsublist fsubrow esubrow psubrow eff
  , RowCons key (Req req) fsubrow frow
  , RowToList frow (Cons key (Req req) fsublist)
  , RowCons key env esubrow erow
  , RowCons key (PerformFetch req eff) psubrow prow
  , IsSymbol key
  , RowLacks key fsubrow
  , RowLacks key esubrow
  , RowLacks key psubrow
  ) ⇒ FetchableRows (Cons key (Req req) fsublist) frow erow prow eff where
  getFetcher _ (env ∷ Record erow) =
      Record.insert key performHead performRest
    where
      key ∷ SProxy key
      key = SProxy

      envHead ∷ env
      envHead = Record.get key env

      envRest ∷ Record esubrow
      envRest = Record.delete key env

      performHead ∷ PerformFetch req eff
      performHead = PerformFetch (fetch envHead)

      performRest ∷ Record psubrow
      performRest = getFetcher (RLProxy ∷ RLProxy fsublist) envRest


instance fetchableFetch ∷
  ( FetchableRows flist frow erow prow eff
  , RowToList frow flist
  ) ⇒ Fetchable (Fetch frow) (Record erow) eff where
  fetch env = splitAndFetch
    where
      fetchers ∷ Record prow
      fetchers = getFetcher (RLProxy ∷ RLProxy flist) env

      fetcherMap ∷ StrMap (PerformFetch FVal eff)
      fetcherMap = unsafeCoerce fetchers
      -- this coerce is okay because both records and strmaps have the same representation

      splitAndFetch ∷ ∀ a. Array (BlockedFetch (Fetch frow) a) → ParAff eff Unit
      splitAndFetch blockedFetches =
         forWithIndex_ blockedFetchGroups \name bfs →
           case StrMap.lookup name fetcherMap of
             Just (PerformFetch pf) → pf bfs
             Nothing → for_ bfs \bf → parallel do
               completeBlockedFetch bf (ThrowPaxl (error ("No fetcher found for Fetch:" <> name)))
        where
          blockedFetchReps ∷ Array (BlockedFetch (FetchRep FVal) a)
          blockedFetchReps = unsafeCoerce blockedFetches

          blockedFetchGroups ∷ StrMap (Array (BlockedFetch FVal a))
          blockedFetchGroups =
            blockedFetchReps
            # Array.foldMap \bf@(BlockedFetch { request: FetchRep { sym, req }, blockedVar }) →
                StrMap.singleton sym [ BlockedFetch { request: req, blockedVar }]


inject ∷ ∀ sym req a r1 r2. RowCons sym (Req req) r1 r2 ⇒ IsSymbol sym ⇒ SProxy sym → req a → Fetch r2 a
inject proxy req =
  (unsafeCoerce ∷ FetchRep req a → Fetch r2 a)
    (FetchRep { sym: reflectSymbol proxy, req })


request ∷ ∀ req eff a. req a → GenPaxl req eff a
request req = GenPaxl \env → do
  blockedFetch ← makeBlockedFetch req
  enqueueBlockedFetch env blockedFetch
    $> Blocked (Cont (awaitBlockedFetch blockedFetch))


awaitBlockedFetch ∷ ∀ req eff a. BlockedFetch req a → GenPaxl req eff a
awaitBlockedFetch (BlockedFetch { blockedVar }) = GenPaxl \_ →
  AVar.readVar blockedVar <#> case _ of
    Ok a → Done a
    ThrowPaxl err → Throw err


enqueueBlockedFetch ∷ ∀ req eff a. Env req eff → BlockedFetch req a → Aff (GenPaxlEffects req :+ eff) Unit
enqueueBlockedFetch { pending } blockedFetch = liftEff do
  modifyRef pending (List.Cons (unsafeCoerce blockedFetch))


makeBlockedFetch ∷ ∀ req eff a. req a → Aff (GenPaxlEffects req :+ eff) (BlockedFetch req a)
makeBlockedFetch req =
  AVar.makeEmptyVar <#> \blockedVar → BlockedFetch { request: req, blockedVar }


completeBlockedFetch ∷ ∀ req eff a. BlockedFetch req a → ResultVal a → Aff eff Unit
completeBlockedFetch (BlockedFetch { blockedVar }) result =
  unsafeCoerceAff $ AVar.putVar result blockedVar


completeBlockedFetchOf ∷ ∀ req eff a b. (a ~ b) → BlockedFetch req a → ResultVal b → Aff eff Unit
completeBlockedFetchOf _ (BlockedFetch { blockedVar }) result =
  unsafeCoerceAff $ AVar.putVar (unsafeCoerce result) blockedVar
