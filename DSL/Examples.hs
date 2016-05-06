module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State


-- | example programs
--
prog1 = do move (up fullPower)
           move (forward fullPower)
           move (right fullPower)
           move (spinL halfPower)
           move (spinR halfPower)
           move (left fullPower)
           move (backward fullPower)
           move (down fullPower)
