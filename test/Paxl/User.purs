module Test.Paxl.User
  ( User(..)
  , Id
  , Job(..)
  , Service
  , UserRequest
  , Env
  , getId
  , getUserIds
  , getUserById
  , getUserByName
  , setUserJob
  , initialUsers
  ) where

import Prelude

import Control.Monad.Aff (error, parallel)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef)
import Data.Array (find, length, mapMaybe) as Array
import Data.Foldable (for_)
import Data.Leibniz (type (~))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.StrMap (fromFoldable, lookup) as StrMap
import Data.String (toLower) as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Paxl (type (+), Paxl)
import Paxl.Fetch (class Fetchable, class Hashable, BlockedFetch(..), Req, ResultVal(..), completeBlockedFetchOf, hash, inject, request)

-- User types

newtype User = User
  { id ∷ Id
  , name ∷ String
  , age ∷ Int
  , job ∷ Job
  }

derive instance newtypeUser ∷ Newtype User _

instance showUser ∷ Show User where
  show (User { id, name, age, job }) =
    name <> " "
    <> "{ id: " <> show id
    <> ", age: " <> show age
    <> ", job: " <> show job
    <> "}"


newtype Id = Id String

instance showId ∷ Show Id where
  show (Id str) = "#" <> str

derive newtype instance eqId ∷ Eq Id


data Job = Firefighter | Scientist | Teacher

instance showJob ∷ Show Job where
  show Firefighter = "Firefighter"
  show Scientist = "Scientist"
  show Teacher = "Teacher"


getId ∷ User → Id
getId (User { id }) = id


changeAge ∷ User → Int → User
changeAge (User u) age = User u { age = age }


changeJob ∷ User → Job → User
changeJob (User u) job = User u { job = job }


-- User Service

type Service reqs = ( userService ∷ Req (UserRequest) | reqs )

type Env = { verbose ∷ Boolean, users ∷ Ref (Array User) }

data UserRequest a
  = GetUserIds (a ~ Array Id)
  | GetUserById (a ~ User) Id
  | GetUserByName (a ~ Maybe User) String
  | SetUserJob (a ~ User) Id Job


getUserIds ∷ ∀ reqs eff. Paxl (Service + reqs) eff (Array Id)
getUserIds = request (inject _userService (GetUserIds id))


getUserById ∷ ∀ reqs eff. Id → Paxl (Service + reqs) eff User
getUserById userId = request (inject _userService (GetUserById id userId))


getUserByName ∷ ∀ reqs eff. String → Paxl (Service + reqs) eff (Maybe User)
getUserByName name = request (inject _userService (GetUserByName id name))


setUserJob ∷ ∀ reqs eff. Id → Job → Paxl (Service + reqs) eff User
setUserJob userId job = request (inject _userService (SetUserJob id userId job))

_userService = SProxy ∷ SProxy "userService"


initialUsers ∷ Array User
initialUsers =
  [ User
      { id: Id "d7069281-9363-4506-8088-f7787a7b2d1e"
      , name: "Alice"
      , age: 24
      , job: Scientist
      }
  , User
      { id: Id "826af9b9-7e2d-492c-a880-68ad949fcbf4"
      , name: "Bob"
      , age: 46
      , job: Teacher
      }
  , User
      { id: Id "b2932c9b-85e9-41f7-91d6-62dd37330e8c"
      , name: "Cheyenne"
      , age: 37
      , job: Firefighter
      }
  ]


instance hashableUserRequest ∷ Hashable UserRequest where
  hash (GetUserIds _) = "GetUserIds"
  hash (GetUserById _ userId) = "GetUserById: " <> show userId
  hash (GetUserByName _ name) = "GetUserByName: " <> name
  hash (SetUserJob _ userId job) = "SetUserJob: " <> show userId <> " " <> show job


instance fetchableUserRequest ∷ Fetchable UserRequest { verbose ∷ Boolean, users ∷ Ref (Array User) } ( console ∷ CONSOLE, ref ∷ REF ) where
  fetch { verbose, users } blockedFetches = parallel do
    log ("UserService executing a batch of " <> show (Array.length blockedFetches) <> " requests")
    -- change all users first
    let jobChanges ∷ StrMap Job
        jobChanges = StrMap.fromFoldable do
          blockedFetches # Array.mapMaybe case _ of
            BlockedFetch { request: SetUserJob _ userId job } → Just (Tuple (show userId) job)
            _ → Nothing
    currentUsers ← liftEff do
      oldUsers ← readRef users
      let newUsers ∷ Array User
          newUsers = oldUsers # map case _ of
            User u | Just newJob ← StrMap.lookup (show u.id) jobChanges →
              User u { job = newJob }
            user → user
      newUsers <$ writeRef users newUsers
    for_ blockedFetches \bf@(BlockedFetch { request, blockedVar }) → do
      when verbose $ log (" -> " <> hash request)
      case request of
        GetUserIds proof →
          completeBlockedFetchOf proof bf (Ok (currentUsers # map \(User { id }) → id))
        GetUserById proof userId →
          completeBlockedFetchOf proof bf
            case Array.find (\(User { id }) → id == userId) currentUsers of
              Just user → Ok user
              Nothing → ThrowPaxl (error ("No user found with id " <> show userId))
        GetUserByName proof name → do
          let nameLower = String.toLower name
          completeBlockedFetchOf proof bf
            $ Ok (Array.find (\(User { name }) → String.toLower name == nameLower) currentUsers)
        SetUserJob proof userId job →
          completeBlockedFetchOf proof bf
            case Array.find (\(User { id }) → id == userId) currentUsers of
              Just user → Ok user
              Nothing → ThrowPaxl (error ("No user found with id " <> show userId))
