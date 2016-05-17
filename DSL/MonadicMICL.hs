module MonadicMICL where

import Control.Monad
import Control.Monad.State

import Reactive.Banana
import Reactive.Banana.Combinators

import MICL


-- | stateful combinator functions
--   movement: takes a signal, and updates the state status.
--   switchAgent: takes a signal, and updates the state status.
--   switchMode: takes a signal, and updates the state status.
--
move :: Device -> Program
move dev sig = do (loc,dis,opm) <- get
                  put (locate dev sig loc,dis,opm)
                  (loc,dis,opm) <- get
                  put (loc,clearWaypoint loc dis,opm)

newTask :: Task -> State Status ()
newTask tsk = do (loc,dis,opm) <- get
                 put (loc,addTask tsk dis,opm)

removeTask :: Task -> State Status ()
removeTask tsk = do (loc,dis,opm) <- get
                    put (loc,deleteTask tsk dis,opm)

updateAgent :: Program
updateAgent sig = do (loc,dis,opm) <- get
                     put (loc,dis,(changeAgent sig,snd opm))

updateMode :: Program
updateMode sig = do (loc,dis,opm) <- get
                    put (loc,dis,(fst opm,changeMode sig))


-- | semantic domain for the state
--
type Program = Signal -> State Status ()
