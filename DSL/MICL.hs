{- | Mixed-Initiative Controller Language (Keeley Abbott -- 2016)
     Description: A programmer is going to give the device or vehicle a series
        of commands to execute in order to navigate a route. The programmer is
        also going to provide a set of protocols for the human agent that
        describe goal-directed tasks for the human agent to complete as well as
        some additional information, and potentially how to recover from some
        exceptions.
-}

module MICL where

-- | floating point values indicate the amount of voltage (power) being
--       transferred to the servos spinning the props.
--   channel: interprets how the drone receives commands (either from the user
--       or the computer agent)
--   mode: indicates whether the drone is in a normal mode, recovering from some
--       exception, in a wait status, or is ownership of the process is being
--       returned to the human actor or the computer.
--   enable: interprets how the drone interprets the progressive commands from the
--       user or the program:
--           * bit 0: enable/disable progressive commands
--           * bit 1: enable/disable combined yaw
--   roll: drone left/right tilt value v = { r | -1.0 \leq r \leq 1.0 }
--           * a negative value will make the drone tilt left, thus move left
--           * a positive value will make the drone tilt right, thus move right
--   pitch: drone front/back tilt value v = { p | -1.0 \leq p \leq 1.0 }
--           * a negative value will make the drone lower it's front, thus move
--             forward
--           * a negative value will make the drone raise it's front, thus move
--             backward
--   gaz: drone vertical speed value v = { g | -1.0 \leq g \leq 1.0 }
--           * a negative value will decrease power to all engines, thus move
--             down
--           * a positive value will increase power to all engines, thus move up
--   yaw: drone angular speed value v = { y | -1.0 \leq y \leq 1.0 }
--           * a negative value will make the drone spin (turn) left
--           * a positive value will make the drone spin (turn) right
--
data Signal = Signal { channel :: Agent
                     , mode :: Mode
                     , enable :: Bool
                     , roll :: Float
                     , pitch :: Float
                     , gaz :: Float
                     , yaw :: Float
                     }
            deriving(Show)

data Agent = Human
           | Computer
           deriving(Show)

data Mode = Recovery
          | Normal
          | Return
          | Wait
          deriving(Show)

type OpMode = (Agent,Mode)


-- | location is calculated as a relative location using dead-reckoning.
--
type Location = (North,East,Down)

type North = Float
type East = Float
type Down = Float


-- | location functions
--   take a signal and a speed, and produce a double that represents the new relative
--       location of the device or vehicle (reported in meters per second).
--   locateNorth: calculates the new location north of the "home" point based on speed
--       and previous location.
--   locateEast: calculates the new location east of the "home" point based on speed
--       and previous location.
--   locateDown: calculates the new location aboce the "home" point based on speed and
--       previous location.
--
--   TODO: need to deal with orientation issues!
--
locateNorth :: Float -> Location -> Location
locateNorth f (n,e,d) = (n + f,e,d)

locateEast :: Float -> Location -> Location
locateEast f (n,e,d) = (n,e + f,d)

locateDown :: Float -> Location -> Location
locateDown f (n,e,d) = (n,e,d + f)


-- | operators and combinators
--   locate: takes a signal and a location, and calculates the drone's new location
--       based on the max speed and the signal input.
--   changeAgent: takes a signal and switches the agent of the drone based on
--       the channel.
--   changeMode: takes a signal and switches the mode of the drone based on the current
--       mode.
--   addTask: takes a task and adds it to the current list of tasks.
--   deleteTask: removes a task from the current list of tasks if it exists.
--   clearWaypoint: clears a waypoint from the list of tasks if it exists.
--
locate :: Signal -> Location -> Location
locate sig loc = (locateNorth ((pitch sig) * rate)
                  (locateEast ((roll sig) * rate)
                   (locateDown ((gaz sig) * rate) loc)))

changeAgent :: Signal -> Agent
changeAgent sig = case (channel sig) of
  Computer -> Human
  Human    -> Computer

changeMode :: Signal -> Mode
changeMode sig = case (mode sig) of
  Recovery -> Normal
  Normal   -> Wait
  Return   -> Wait
  Wait     -> Normal

changeEnable :: Signal -> Signal
changeEnable sig = case (enable sig) of
  True -> sig { enable = False }
  _    -> sig { enable = True }

addTask :: Task -> Display -> Display
addTask (Left  wpt) dis = dis ++ [Left wpt]
addTask (Right ins) dis = dis ++ [Right ins]

deleteTask :: Task -> Display -> Display
deleteTask _          []     = []
deleteTask (Left loc) (d:ds) = case d of
  (Left loc')
    | loc       == loc' -> ds
    | otherwise         -> d : deleteTask (Left loc) ds
  _ -> d : deleteTask (Left loc) ds
deleteTask (Right str)      (d:ds) = case d of
  (Right str')
    | str       == str' -> ds
    | otherwise         -> d : deleteTask (Right str) ds
  _ -> d : deleteTask (Right str) ds

clearWaypoint :: Location -> Display -> Display
clearWaypoint _   []     = []
clearWaypoint loc (d:ds) = case d of
  (Left loc')
    | loc       == loc' -> ds
    | otherwise         -> d : clearWaypoint loc ds
  _ -> d : clearWaypoint loc ds


-- | semantic domain
--
type Domain = Signal -> Status Condition -> Status Condition

data Status a = S a Timestamp [Status a]
              deriving(Show)

type Condition = (Location,Display,OpMode)
type Timestamp = Int

instance Functor Status where
  fmap f (S c t []) = S (f c) t []
  fmap f (S c t ss) = S (f c) t (map (fmap f) ss)

instance Applicative Status where
  pure c                      = S c 0 []
  (S f t []) <*> (S c t' [])  = S (f c) (t + t') []
  (S f t ss) <*> (S c t' ss') = S (f c) (t + t') [q <*> p | q <- ss, p <- ss']

timestamp :: Status a -> Timestamp
timestamp (S c t []) = t
timestamp (S c t ss) = sum (map timestamp ss)

addTime :: Timestamp -> Status a -> Status a
addTime t (S c t' []) = S c (t + t') []
addTime t (S c t' ss) = S c (t + t') (map (addTime t) ss)


-- | display type synonyms
--
type Display = [Task]
type Task = Either Waypoint Instruction
type Waypoint = Location
type Instruction = String


-- | default values
--
signalDefault = Signal { channel = (fst opModeDefault)
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

statusDefault = (homeLocation,displayDefault,opModeDefault)


-- | smart constructors
--
ascend f = signalDefault { gaz = f }
descend f = signalDefault { gaz = (negate f) }
strafeL f = signalDefault { roll = (negate f) }
strafeR f = signalDefault { roll = f }
forward f = signalDefault { pitch = f }
backward f = signalDefault { pitch = (negate f) }
spinL f = signalDefault { enable = disabled
                       , yaw = (negate f)
                       }
spinR f = signalDefault { enable = disabled
                       , yaw = f
                       }

north :: Float -> Float
north f = f

east :: Float -> Float
east f = f

down :: Float -> Float
down f = f


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

rate :: Float
rate = 2.0
