module MonadicMICL where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State

import Reactive.Banana
import Reactive.Banana.Combinators

import MICL


-- | stateful operators and combinators
--   movement: takes a signal, and updates the state status.
--   switchAgent: takes a signal, and updates the state status.
--   switchMode: takes a signal, and updates the state status.
--
move :: Program
move sig = do (loc,dis,opm) <- get
              put (locate sig loc,dis,opm)
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

to :: Signal -> Float -> State Status ()
to = undefined

while :: Program -> Program -> Program
while = undefined


-- | iterative operator for programs (or should this be iterative for
--       status?)
-- (:+:)

-- | blended operator for programs
-- (:&:)


-- | semantic domain for the state
--
type Program = Signal -> State Status ()
