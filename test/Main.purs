module Test.Main (main) where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef)
import Data.Traversable (for, for_)
import Paxl (type (+), type (:+), Par(..), Paxl, PaxlEffects, initEnv, runPaxl)
import Test.Paxl.DelayedEcho (DelayedEcho)
import Test.Paxl.DelayedEcho (print) as DelayedEcho
import Test.Paxl.User (User)
import Test.Paxl.User as User


program ∷ ∀ eff. Paxl (DelayedEcho + User.Service + ()) eff Unit
program = do
  DelayedEcho.print "Example program 1"
  DelayedEcho.print "Example program 2"
  users ← getUsers
  Par <$ for users \user → do
    DelayedEcho.print (show user)
  newUsers ← for users \user → do
    makeScientist user
  for_ newUsers \user → do
    DelayedEcho.print (show user)
  pure unit


getUsers ∷ ∀ reqs eff. Paxl (User.Service + reqs) eff (Array User)
getUsers = do
  userIds ← User.getUserIds
  for userIds \userId → do
    User.getUserById userId


makeScientist ∷ ∀ reqs eff. User → Paxl (User.Service + reqs) eff User
makeScientist user =
  User.setUserJob (User.getId user) User.Scientist


main :: forall e. Eff (PaxlEffects (DelayedEcho + User.Service + ()) :+ e) Unit
main = do
  users ← newRef User.initialUsers
  let env =
        { delayedEcho: { verbose: false, prefix: "[DEBUG] " }
        , userService: { verbose: true, users }
        }
  programEnv ← initEnv env
  launchAff_ $ runPaxl programEnv program
