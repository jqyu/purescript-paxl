module Test.Main (main) where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef)
import Data.Traversable (for, for_)
import Paxl (type (+), type (:+), Par(..), Paxl, PaxlEffects, initEnv, runPaxl, (|>))
import Test.Paxl.DelayedEcho (DelayedEcho)
import Test.Paxl.DelayedEcho (print) as DelayedEcho
import Test.Paxl.User (User)
import Test.Paxl.User as User


program ∷ Paxl (DelayedEcho + User.Service + ()) Par
program = do
  DelayedEcho.print "Example program 1"
  DelayedEcho.print "Example program 2"
  users ← getUsers
  printUsers(users)
  for_ users \user → do
    makeScientist user
  newUsers ← getUsers
  printUsers(newUsers)


getUsers ∷ ∀ reqs. Paxl (User.Service + reqs) (Array User)
getUsers = do
  userIds ← User.getUserIds
  for userIds \userId → do
    User.getUserById userId


printUsers ∷ Array User → ∀ reqs. Paxl (DelayedEcho + reqs) Par
printUsers users =
  Par <$ for users \user → do
    DelayedEcho.print (show user)


makeScientist ∷ User → ∀ reqs. Paxl (User.Service + reqs) User
makeScientist user = do
  let userId = User.getId user
  User.setUserJob userId User.Scientist


main ∷ ∀ e. Eff (PaxlEffects (DelayedEcho + User.Service + ()) :+ e) Unit
main = do
  users ← newRef User.initialUsers
  let env =
        { delayedEcho: { verbose: false, prefix: "[DEBUG] " }
        , userService: { verbose: true, users }
        }
  programEnv ← initEnv env
  runPaxl programEnv program
    |> launchAff_
