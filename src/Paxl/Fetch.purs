module Paxl.Fetch
  ( class Fetchable
  , class FetchableRows
  , Fetch
  , Req
  , getFetcher
  , fetch
  , request
  , requestCached
  , completeBlockedFetch
  , completeBlockedFetchOf
  , module ReExports
  ) where

import Paxl.Prelude

import Control.Monad.Aff (Aff, ParAff, error, parallel)
import Control.Monad.Aff.AVar (AVar)
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
import Paxl.Effect (GenPaxlEffects)
import Paxl.Monad (GenPaxl(..), Env, Result(..), Cont(..), Val)
import Paxl.RequestStore (BlockedFetch(..), CacheResult(..))
import Paxl.RequestStore (class Cacheable, Result(..), cacheKey, peek, poke, prefixKey) as RequestStore
import Paxl.RequestStore (class Cacheable, BlockedFetch(..), Result(..), Key(..), cacheKey) as ReExports
import Type.Prelude (class RowLacks)
import Type.Row (class RowToList, RLProxy(..), Nil, Cons)
import Unsafe.Coerce (unsafeCoerce)


class Fetchable req env eff | req → env, req → eff where
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
               completeBlockedFetch bf
                 ◁ RequestStore.Throw (error ("No fetcher found for Fetch:" ⬦ name))
        where
          blockedFetchReps ∷ Array (BlockedFetch (FetchRep FVal) a)
          blockedFetchReps = unsafeCoerce blockedFetches

          blockedFetchGroups ∷ StrMap (Array (BlockedFetch FVal a))
          blockedFetchGroups =
            blockedFetchReps
            ▷ Array.foldMap case _ of
                BlockedFetch { request: FetchRep { sym, req }, blockedVar } →
                  StrMap.singleton sym [ BlockedFetch { request: req, blockedVar } ]


inject ∷ ∀ sym req a r1 r2. RowCons sym (Req req) r1 r2 ⇒ IsSymbol sym ⇒ SProxy sym → req a → Fetch r2 a
inject proxy req =
  FetchRep { sym: reflectSymbol proxy, req }
    ▷ (unsafeCoerce ∷ FetchRep req a → Fetch r2 a)


request ∷ ∀ sym req a r1 r2. RowCons sym (Req req) r1 r2 ⇒ IsSymbol sym ⇒ SProxy sym → req a → GenPaxl (Fetch r2) a
request proxy req = GenPaxl \env → do
  blockedFetch ← makeBlockedFetch (inject proxy req)
  enqueueBlockedFetch env blockedFetch
    $> Blocked (Cont (awaitBlockedFetch blockedFetch))


requestCached ∷ ∀ sym req a r1 r2. RowCons sym (Req req) r1 r2 ⇒ IsSymbol sym ⇒ RequestStore.Cacheable req ⇒ SProxy sym → req a → GenPaxl (Fetch r2) a
requestCached proxy req = GenPaxl \env → do
  let key = RequestStore.prefixKey proxy (RequestStore.cacheKey req)
  cacheResult ← liftEff do
    RequestStore.peek env.requestStore key
  case cacheResult of
    Resolved (RequestStore.Ok a) →
      pure ◁ Done ((unsafeCoerce ∷ Val → a) a)
    Resolved (RequestStore.Throw err) →
      pure ◁ Throw err
    Waiting blockedVar →
      pure ◁ Blocked (Cont (awaitBlocked (unsafeCoerce blockedVar)))
    Uncached → do
      blockedFetch ← makeBlockedFetch ◁ inject proxy req
      liftEff do
        blockedFetch
          ▷ (unsafeCoerce ∷ BlockedFetch (Fetch r2) a → BlockedFetch (Fetch r2) Val)
          ▷ RequestStore.poke env.requestStore key
      enqueueBlockedFetch env blockedFetch
        $> Blocked (Cont (awaitBlockedFetch blockedFetch))


awaitBlockedFetch ∷ ∀ req a. BlockedFetch req a → GenPaxl req a
awaitBlockedFetch (BlockedFetch { blockedVar }) = awaitBlocked blockedVar


awaitBlocked ∷ ∀ req a. AVar (RequestStore.Result a) → GenPaxl req a
awaitBlocked blockedVar = GenPaxl \_ →
  AVar.readVar blockedVar ▷ map case _ of
    RequestStore.Ok a → Done a
    RequestStore.Throw err → Throw err


enqueueBlockedFetch ∷ ∀ req a. Env req → BlockedFetch req a → Aff (GenPaxlEffects req ()) Unit
enqueueBlockedFetch { pending } blockedFetch = liftEff do
  modifyRef pending (List.Cons (unsafeCoerce blockedFetch))


makeBlockedFetch ∷ ∀ req a. req a → Aff (GenPaxlEffects req ()) (BlockedFetch req a)
makeBlockedFetch req =
  AVar.makeEmptyVar ▷ map \blockedVar → BlockedFetch { request: req, blockedVar }


completeBlockedFetch ∷ ∀ req eff a. BlockedFetch req a → RequestStore.Result a → Aff eff Unit
completeBlockedFetch (BlockedFetch { blockedVar }) result =
  unsafeCoerceAff ◁ AVar.putVar result blockedVar


completeBlockedFetchOf ∷ ∀ req eff a b. (a ~ b) → BlockedFetch req a → RequestStore.Result b → Aff eff Unit
completeBlockedFetchOf _ (BlockedFetch { blockedVar }) result =
  unsafeCoerceAff ◁ AVar.putVar (unsafeCoerce result) blockedVar
