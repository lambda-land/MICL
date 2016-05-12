module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State


-- | example programs
--   rmx in let statement is the maximum rate achievable by the vehicle
--       or device in meters/second.
prog1 = do move (ascend fullPower)
           move (left fullPower)
           newTask (Left (waypoint,Location { north = 0.0
                                            , east = 0.0
                                            , down = 0.0
                                            } ))
           move (forward fullPower)
           move (right fullPower)
           move (backward fullPower)
           move (descend fullPower)
           newTask (Left (waypoint,Location { north = 150.0
                                            , east = -25.0
                                            , down = 75.0
                                            } ))
