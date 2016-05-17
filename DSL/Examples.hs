module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State


-- | example programs
--
squareFlight :: State Status ()
squareFlight = let drone = Device { gps = disabled
                                  , rate = 25.0
                                  }
               in
                 do move drone (ascend fullPower)
                    move drone (forward fullPower)
                    move drone (strafeL fullPower)
                    newTask (Left (north 0.0,east 0.0,down 0.0))
                    move drone (strafeR fullPower)
                    newTask (Right "Remove this task once you land.")
                    move drone (backward fullPower)
                    move drone (descend fullPower)
                    removeTask (Right "Remove this task once you land.")
