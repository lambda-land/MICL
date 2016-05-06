module MonadicMICL where

import Control.Event.Handler
import Control.Monad
import Control.Monad.State

import MICL


-- | stateful combinator functions
--   movement: takes a signal, and updates the state status
--   userDisplay: takes a signal, and updates the state status
--   switchAgent: takes a signal, and updates the state status
--   switchMode: takes a signal, and updates the state status
--
move :: Program
move sig = do (loc,dis,opm) <- get
              put (relocate sig loc,dis,opm)
              return (movement sig)

updateDisplay :: Program
updateDisplay sig  = do (loc,dis,opm) <- get
                        put (loc,(display sig dis),opm)
                        return (movement sig)

updateAgent :: Program
updateAgent sig = do (loc,dis,opm) <- get
                     put (loc,dis,(switchAgent sig,snd opm))
                     return (movement sig)

updateMode :: Program
updateMode sig = do (loc,dis,opm) <- get
                    put (loc,dis,(fst opm,switchMode sig))
                    return (movement sig)


-- | semantic domain for the state
--
type Program = Signal -> State Status Servos
