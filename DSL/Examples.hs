module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State


-- | example programs
--
takeOff f = do ascend fullPower `to` meters f

land = do descend fullPower `to` meters 0

takeOffAndLand = do takeOff (meters 250)
                    land

squareFlight = do takeOff (meters 250)
                  (move `at` (seconds 125)) (ascend fullPower)
                  strafeR fullPower `to` meters 250
                  forward fullPower `to` meters 250
                  newTask (waypoint (north 0.0,east 0.0,down 0.0))
                  strafeL fullPower `to` meters (negate 250)
                  backward fullPower `to` meters (negate 250)
                  newTask (instruction "return the drone to its home waypoint.")

simpleForward = do forward fullPower `to` meters 10

simpleStrafeR = do strafeR fullPower `to` meters 10

simpleGrid = do takeOff (meters 10)
                simpleForward
                simpleStrafeR
                untilW [ backward fullPower
                       , strafeL fullPower ]
                  (north (meters 8),east (meters 2),down (meters 10))
                simpleStrafeR
                untilW [ backward fullPower
                       , strafeL fullPower ]
                  (north (meters 6),east (meters 2),down (meters 10))
                simpleStrafeR
                untilW [ backward fullPower
                       , strafeL fullPower ]
                  (north (meters 4),east (meters 2),down (meters 10))
                simpleStrafeR
                untilW [ backward fullPower
                       , strafeL fullPower ]
                  (north (meters 2),east (meters 2),down (meters 10))
                simpleStrafeR
                untilW [ backward fullPower
                       , strafeL fullPower ]
                  (north (meters 0),east (meters 0),down (meters 10))
                land

-- interactiveFlight = do takeOff (meters 250)
--                        forward fullPower `to` meters 250
--                        strafeR fullPower `to` meters 250

--                        newTask (waypoint (north 0,east 0,down 0))
--                        newTask (instruction "return the drone to its home waypoint.")

--                        updateOpMode (wait Human)

--                        while (untilW [backward fullPower,strafeL fullPower]
--                               (north 0,east 0,down 250))

--                          untilO interaction (Computer,Wait)

--                        land
