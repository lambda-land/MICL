module MonadicMICL where

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
move sig = do (srv,dis,opm) <- get
              put ((movement sig srv),dis,opm)

updateDisplay :: Program
updateDisplay sig = do (srv,dis,opm) <- get
                       put (srv,(display sig dis),opm)

updateAgent :: Program
updateAgent sig = do (srv,dis,opm) <- get
                     put (srv,dis,(switchAgent sig,snd opm))

updateMode :: Program
updateMode sig = do (srv,dis,opm) <- get
                    put (srv,dis,(fst opm,switchMode sig))


-- | semantic domain for the state
--
type Program = Signal -> State Status ()
