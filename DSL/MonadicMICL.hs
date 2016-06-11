module MonadicMICL where

import Control.Monad
import Control.Monad.State

import MICL


-- | stateful operators and combinators
--   movement: takes a signal, and updates the state status.
--   switchAgent: takes a signal, and updates the state status.
--   switchMode: takes a signal, and updates the state status.
--
move :: Program Input
move sig = do (t,(loc,dis,opm)) <- get
              put (time (addTime tick (t,(loc,dis,opm)))
                  ,(locate sig loc,dis,opm))
              (t,(loc,dis,opm)) <- get
              put (t,(loc,clearWaypoint loc dis,opm))

newTask :: Program Output
newTask sig = do (t,(loc,dis,opm)) <- get
                 put (t,(loc,addTask sig dis,opm))

removeTask :: Program Output
removeTask sig = do (t,(loc,dis,opm)) <- get
                    put (t,(loc,deleteTask sig dis,opm))

updateAgent :: Program Input
updateAgent sig = do (t,(loc,dis,opm)) <- get
                     put (t,(loc,dis,(changeAgent opm,snd opm)))

updateMode :: Program Input
updateMode sig = do (t,(loc,dis,opm)) <- get
                    put (t,(loc,dis,(fst opm,changeMode opm)))


-- given a signal and a desired ending location, move until the
--   location matches the desired one
to :: Input -> Float -> State (Time,Status) ()
to inp f = do (t,s) <- get
              case (projectLoc inp s /= f) of
                True  -> move inp >> to inp f
                False -> return ()

-- this is to inject an instruction, or change the status at a
--   given time
at :: Program Input -> Time -> Input -> State (Time,Status) ()
at prg t inp = do (t',s) <- get
                  case (t == t') of
                    True  -> prg inp
                    False -> return ()

interrupt :: Program Input -> Time -> Input -> State (Time,Status) ()
interrupt = undefined

while :: Program a -> Program a -> Program a
while p1 p2 a = do (t,s) <- get
                   p2 a >> p1 a
                   return ()

untilW :: [Input] -> Waypoint -> State (Time,Status) ()
untilW []     wpt = do return ()
untilW (i:is) wpt = do (t,(loc,dis,opm)) <- get
                       case (wpt == loc) of
                         False -> i `to` (projectLoc i (wpt,dis,opm)) >> untilW is wpt
                         True  -> return ()

untilT :: [Input] -> Time -> State (Time,Status) ()
untilT []     tme = do return ()
untilT (i:is) tme = do (t,s) <- get
                       case (t == tme) of
                         False -> i `to` (projectLoc i s) >> untilT is tme
                         True  -> return ()

untilO :: Input -> OpMode -> State (Time,Status) ()
untilO inp opm = do (t,(loc,dis,opm')) <- get
                    case (opm' == opm) of
                      False -> updateMode inp
                      True  -> return ()


-- | semantic domain for the state
--
type Program a = a -> State (Time,Status) ()

-- | helper functions
--
projectLoc :: Input -> Status -> Float
projectLoc inp (loc,_,_) = let (n,e,d) = loc in
  if (pitch inp) /= 0
  then n
  else if (roll inp) /= 0
       then e
       else if (gaz inp) /= 0
            then d
            else 0

addTime :: Time -> (Time,Status) -> (Time,Status)
addTime t (t',s) = (t + t',s)

time :: (Time,Status) -> Time
time (t,s) = t


-- | default values
--
inputDefault = Input { channel = (fst opModeDefault)
                     , mode    = (snd opModeDefault)
                     , enable  = enabled
                     , roll    = zeroPower
                     , pitch   = zeroPower
                     , gaz     = zeroPower
                     , yaw     = zeroPower
                     }

displayDefault = []

opModeDefault = (Computer,Normal)

homeLocation = (north 0.0,east 0.0,down 0.0)

type Time = Double

statusDefault = (startTime,(homeLocation,displayDefault,opModeDefault))


-- | smart constructors
--

-- movement signals
ascend f = inputDefault { gaz = f }
descend f = inputDefault { gaz = (negate f) }
strafeL f = inputDefault { roll = (negate f) }
strafeR f = inputDefault { roll = f }
forward f = inputDefault { pitch = f }
backward f = inputDefault { pitch = (negate f) }
spinL f = inputDefault { enable = disabled
                       , yaw = (negate f)
                       }
spinR f = inputDefault { enable = disabled
                       , yaw = f
                       }

-- display signals
waypoint l = Left l
instruction i = Right i

-- opmode signals
-- opmodes
wait a = inputDefault { channel = a
                      , mode    = Wait
                      }
reinstate a = inputDefault { channel = a
                           , mode    = Return
                           }
exception a = inputDefault { channel = a
                           , mode    = Recovery
                           }

-- dead-reckoning directions
north :: Float -> Float
north f = f

east :: Float -> Float
east f = f

down :: Float -> Float
down f = f

-- time increments
seconds :: Time -> Time
seconds s = s

-- distance
meters :: Distance -> Distance
meters m = m

type Distance = Float


-- | constructed values
--
fullPower :: Float
fullPower = 1.0

threeQtrPower :: Float
threeQtrPower = 0.75

halfPower :: Float
halfPower = 0.5

quarterPower :: Float
quarterPower = 0.25

zeroPower :: Float
zeroPower = 0.0

enabled = True
disabled = False

tick :: Double
tick = 1

startTime :: Double
startTime = 0
