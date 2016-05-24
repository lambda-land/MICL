module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State


-- | example programs
--
takeOff :: State Status ()
takeOff = do ascend fullPower `to` 250

land :: State Status ()
land = do descend halfPower `to` 0

takeOffAndLand :: State Status ()
takeOffAndLand = do takeOff
                    land

moveLeft :: State Status ()
moveLeft = do strafeL fullPower `to` 250

moveRight :: State Status ()
moveRight = do strafeR fullPower `to` 250

squareFlight :: State Status ()
squareFlight = do takeOff
                  move  (forward fullPower)
                  moveLeft
                  newTask (Left (north 0.0,east 0.0,down 0.0))
                  moveRight
                  newTask (Right "Remove this task once you land.")
                  move  (backward fullPower)
                  land
