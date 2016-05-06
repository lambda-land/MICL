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
data Signal = Signal { channel :: Agent,
                       mode :: Mode,
                       enable :: Bool,
                       roll :: Float,
                       pitch :: Float,
                       gaz :: Float,
                       yaw :: Float }
            deriving(Show)

data Agent = Human
           | Computer
           deriving(Show)

data Mode = Recovery
          | Normal
          | Return
          | Wait
          deriving(Show)

data Servos = Servos { leftFront :: Float,
                       rightFront :: Float,
                       leftRear :: Float,
                       rightRear :: Float }
            deriving(Show)

type Display = [String]

type OpMode = (Agent,Mode)


-- | a location can either be obtained as a relative location based on the starting
--       point of the drone, or from an internal or attached GPS unit.
--
data Location = Location { relativeLocation :: (North,East,Down),
                           absoluteLocation :: (Latitude,Longitude,Elevation)
                         }
              deriving(Show)

type North = Float
type East = Float
type Down = Float
type Latitude = Float
type Longitude = Float
type Elevation = Float

type Speed = Float


-- | movement functions
--   take a signal and servos apply the appropriate servo changes (the range of input
--       from the signal is -1.0 to 1.0).
--   moveLat: moves the drone latitudinally (dips the left or right side of the
--       drone).
--   moveLong: moves the drone longitudinally (dips the front or rear side of
--       the drone).
--   moveVert: moves the drone vertically (increases or decreases speed to all
--       of the servos).
--   moveSpin: spins the drone in place either left or right.
--
moveLat :: Float -> Servos -> Servos
moveLat f srv
  | f <  0    = srv { rightFront = f,
                      rightRear  = f
                    }
  | f > 0     = srv { leftFront = f,
                      leftRear  = f
                    }
  | otherwise = srv

moveLong :: Float -> Servos -> Servos
moveLong f srv
  | f < 0     = srv { leftRear  = f,
                      rightRear = f
                    }
  | f > 0     = srv { leftFront  = f,
                      rightFront = f
                    }
  | otherwise = srv

moveVert :: Float -> Servos -> Servos
moveVert f srv
  | f /= 0    = srv { leftFront  = f,
                      rightFront = f,
                      leftRear   = f,
                      rightRear  = f
                    }
  | otherwise = srv

moveSpin :: Float -> Servos -> Servos
moveSpin f srv
  | f < 0     = srv { leftFront = (negate f),
                      rightRear = (negate f)
                    }
  | f > 0     = srv { rightFront = f,
                      leftRear   = f
                    }
  | otherwise = srv


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
locateNorth :: Float -> Speed -> Location -> Location
locateNorth f spd loc = let (n,e,d) = (relativeLocation loc) in
  loc { relativeLocation = (n + (spd * f),e,d) }

locateEast :: Float -> Speed -> Location -> Location
locateEast f spd loc = let (n,e,d) = (relativeLocation loc) in
  loc { relativeLocation = (n,e + (spd * f),d) }

locateDown :: Float -> Speed -> Location -> Location
locateDown f spd loc = let (n,e,d) = (relativeLocation loc) in
  loc { relativeLocation = (n,e,d + (spd * f)) }


-- | combinator functions
--   move: takes a signal and servos and updates the servos
--   display: takes a signal and a display and updates the display
--   updateAgent: takes a signal and switches the agent of the drone based on
--       the channel
--   updateMode: takes a status and updates it based on the current mode of the
--       drone
--
movement :: Signal -> Servos
movement sig = (moveLat (roll sig)
                (moveLong (pitch sig)
                 (moveVert (gaz sig)
                  (moveSpin (yaw sig) servosDefault))))

display :: Signal -> Display -> Display
display sig []  = error "no task to display"
display sig dis = dis

switchAgent :: Signal -> Agent
switchAgent sig = case (channel sig) of
  Computer -> Human
  Human    -> Computer

switchMode :: Signal -> Mode
switchMode sig = case (mode sig) of
  Return -> Wait
  _      -> Normal

switchEnable :: Signal -> Signal
switchEnable sig = case (enable sig) of
  True -> sig { enable = False }
  _    -> sig { enable = True }

relocate :: Signal -> Location -> Location
relocate sig loc = (locateNorth (pitch sig) speed
                    (locateEast (roll sig) speed
                     (locateDown (gaz sig) speed loc)))


-- | semantic domain
--
type Domain = Signal -> Status -> Status

type Status = (Location,Display,OpMode)


-- | default values
--
signalDefault = Signal { channel = Computer,
                         mode    = Normal,
                         enable  = True,
                         roll    = zeroPower,
                         pitch   = zeroPower,
                         gaz     = zeroPower,
                         yaw     = zeroPower
                       }

servosDefault = Servos { leftFront  = zeroPower,
                         rightFront = zeroPower,
                         leftRear   = zeroPower,
                         rightRear  = zeroPower
                       }

displayDefault = []

opModeDefault = (Computer,Normal)

homeLocation = Location { relativeLocation = (0,0,0),
                          absoluteLocation = (0,0,0)
                        }

statusDefault = (homeLocation,displayDefault,opModeDefault)

speed = 15


-- | smart constructors
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

up f = signalDefault { gaz = f }
down f = signalDefault { gaz = (negate f) }
left f = signalDefault { roll = (negate f) }
right f = signalDefault { roll = f }
forward f = signalDefault { pitch = (negate f) }
backward f = signalDefault { pitch = f }
spinL f = signalDefault { enable = False, yaw = (negate f) }
spinR f = signalDefault { enable = False, yaw = f }
