module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State


-- | example programs
--
prog1 = do move (ascend fullPower)
           move (forward fullPower)
           move (left fullPower)
           newTask (Left (waypoint,(north 0.0,east 0.0,down 0.0)))
           move (right fullPower)
           move (backward fullPower)
