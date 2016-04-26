module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State

prog1 = do move (up fullPower)
           move (up halfPower)
           move (down fullPower)
           move (forward quarterPower)
           move (right fullPower)
           move (down halfPower)

prog2 = do move (up fullPower)
           move (forward fullPower)
           move (right fullPower)
           move (backward fullPower)
           move (left fullPower)
           move (down fullPower)

prog3 = do move (up fullPower)
           move (spinL fullPower)
           move (spinR fullPower)
