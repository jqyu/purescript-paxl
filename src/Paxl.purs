module Paxl
  ( PAXL
  , Paxl
  , PaxlEffects
  , module ReExports
  ) where

import Paxl.Effect (GENPAXL, GenPaxlEffects)
import Paxl.Fetch (Fetch)
import Paxl.Monad (GenPaxl)

-- re-exports
import Paxl.Run (initEnv, runPaxl) as ReExports
import Paxl.Prelude as ReExports


type Paxl reqs = GenPaxl (Fetch reqs)

type PAXL reqs = GENPAXL (Fetch reqs)

type PaxlEffects reqs eff = GenPaxlEffects (Fetch reqs) eff
