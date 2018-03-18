module Test.Paxl.DelayedEcho
  ( DelayedEcho
  , DelayedEchoRequest
  , Env
  , print
  , test2
  ) where

import Prelude

import Control.Monad.Aff (Milliseconds(Milliseconds), delay, launchAff_, parallel, sequential)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Data.Array ((..))
import Data.Array (length) as Array
import Data.Int (toNumber) as Int
import Data.Leibniz (type (~))
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Paxl (type (+), type (:+), Par(..), Paxl, PaxlEffects, initEnv, runPaxl, (<|), (|>))
import Paxl.Fetch (class Fetchable, class Hashable, Req, BlockedFetch(..), ResultVal(..), completeBlockedFetchOf, inject, request)


type DelayedEcho reqs = ( delayedEcho ∷ Req DelayedEchoRequest | reqs )

type Env =
  { verbose ∷ Boolean
  , prefix ∷ String
  }


print ∷ ∀ reqs. String → Paxl (DelayedEcho + reqs) Par
print message = Par <$ string { message, milliseconds: 0 }


string ∷ ∀ reqs. { message ∷ String, milliseconds ∷ Int } → Paxl (DelayedEcho + reqs) String
string payload = request <| inject _delayedEcho <| DelayString id payload


int ∷ ∀ reqs. { message ∷ Int, milliseconds ∷ Int } → Paxl (DelayedEcho + reqs) Int
int payload = request <| inject _delayedEcho <| DelayInt id payload


_delayedEcho = SProxy ∷ SProxy "delayedEcho"


data DelayedEchoRequest a
  = DelayString (a ~ String) { message ∷ String, milliseconds ∷ Int }
  | DelayInt (a ~ Int) { message ∷ Int, milliseconds ∷ Int }


instance hashableDelayedEcho ∷ Hashable DelayedEchoRequest where
  hash (DelayString _ { message, milliseconds }) = "S" <> show milliseconds <> ":" <> message
  hash (DelayInt _ { message, milliseconds }) = "I" <> show milliseconds <> ":" <> show message


instance fetchableDelayedEcho ∷ Fetchable DelayedEchoRequest { verbose ∷ Boolean, prefix ∷ String } ( console :: CONSOLE | eff ) where
  fetch { verbose, prefix } blocked = parallel do
    when verbose do
      log (prefix <> "executing batch of " <> show (Array.length blocked) <> " requests")
    sequential <| for_ blocked \bf@(BlockedFetch { request }) →
      parallel case request of
        DelayString proof { message, milliseconds } → do
          when verbose do
            log (prefix <> "> echoing string after " <> show milliseconds <> "ms: " <> show message)
          delay (Milliseconds (Int.toNumber milliseconds))
          if verbose
            then log (prefix <> "< done echoing string after " <> show milliseconds <> "ms: " <> show message)
            else log (prefix <> message <> "(" <> show milliseconds <> "ms)")
          completeBlockedFetchOf proof bf (Ok message)
        DelayInt proof { message, milliseconds } → do
          when verbose do
            log (prefix <> "> echoing int after " <> show milliseconds <> "ms: " <> show message)
          delay (Milliseconds (Int.toNumber milliseconds))
          if verbose
            then log (prefix <> "< done echoing int after " <> show milliseconds <> "ms: " <> show message)
            else log (prefix <> show message <> "(" <> show milliseconds <> "ms)")
          completeBlockedFetchOf proof bf (Ok message)


type Foo reqs = ( fooEchoService ∷ Req DelayedEchoRequest | reqs )

type Bar reqs = ( barEchoService ∷ Req DelayedEchoRequest | reqs )


fooEchoService = SProxy ∷ SProxy "fooEchoService"
barEchoService = SProxy ∷ SProxy "barEchoService"


fooDelayString ∷ ∀ reqs. String → Int → Paxl (Foo + reqs) String
fooDelayString message milliseconds =
  request (inject fooEchoService (DelayString id { message, milliseconds }))


fooDelayInt ∷ ∀ reqs. Int → Int → Paxl (Foo + reqs) Int
fooDelayInt message milliseconds =
  request (inject fooEchoService (DelayInt id { message, milliseconds }))


barDelayString ∷ ∀ reqs. String → Int → Paxl (Bar + reqs) String
barDelayString message milliseconds =
  request (inject barEchoService (DelayString id { message, milliseconds }))


barDelayInt ∷ ∀ reqs. Int → Int → Paxl (Bar + reqs) Int
barDelayInt message milliseconds =
  request (inject barEchoService (DelayInt id { message, milliseconds }))



test2 ∷ ∀ eff. Eff (PaxlEffects (Foo + Bar + ()) :+ eff) Unit
test2 = do
  let userEnv =
       { fooEchoService: { verbose: true, prefix: "[FOO] " }
       , barEchoService: { verbose: true, prefix: "[BAR] " }
       }
  paxlEnv ← initEnv userEnv
  runPaxl paxlEnv test2Paxl
    |> launchAff_


test2Paxl ∷ Paxl (Foo + Bar + ()) Unit
test2Paxl = do
  outer ← barDelayString "Initial" 1000
  for_ (1..5) \i → do
    let milliseconds = i * 100
    if i `mod` 2 == 0
      then void do
        fooDelayString ("OuterLoop: " <> show i) milliseconds
      else void do
        barDelayInt i milliseconds
    for_ (1..5) \j → do
      let val = i * 10 + j
      let milliseconds = val * 10
      if j `mod` 2 == 0
        then void do
          barDelayString ("InnerLoop: " <> show val) milliseconds
        else void do
          fooDelayInt val milliseconds
