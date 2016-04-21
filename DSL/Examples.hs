module Examples where

import MICL
import MonadicMICL

import Control.Monad
import Control.Monad.State

prog1 = do move (vert fullPower)
           move (vert fullPower)
           move (vert halfPower)
           move (lat (negate fullPower))
           move (spin quarterPower)
