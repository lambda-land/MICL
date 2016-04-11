module MICL where

-- | programmer is going to give the device or vehicle a series of commands to
--   execute in order to navigate a route. the programmer is also going to
--   provide a set of protocols to the human actor that describes goal-directed
--   tasks for the human actor to complete as well as how to recover from some
--   potential exceptions.
--
-- | a message is an output to the drone or vehicle.
--   up/down: a value v = { e \forall e \in \mathbb{R} } in meters
--   clock/counter: a value v = { c | -180.0 \lt c \lt 180.0 } in degrees
--   forward/backward: a value v = { f \forall f \in \mathbb{R} } in meters
--   strafe left/right: a value v = { s \forall s \in \mathbb{R} } in meters
--   turn left/right: a value v = { t | -180.0 \lt t \lt 180.0 } in degrees
--   protocol: a string with goal-directed task(s) for the user to complete, or
--     additional information needed to complete the program
--
data Message = Up Float
             | Down Float
             | Clockwise Float
             | CounterClock Float
             | Forward Float
             | Backward Float
             | StrafeLeft Float
             | StrafeRight Float
             | TurnLeft Float
             | TurnRight Float
             | Protocol String
             deriving(Show)

data OpMode = Human Flag
            | System Flag
            deriving(Show)

type Flag = Bool

-- | proximity: a value v = { o | o \geq 0.0 } in meters
--   altitude: a value v = { a | a \geq 0.0 } in meters
--   axis: a value v = { x | -180.0 \lt x \lt 180.0 } in degrees
--
data Signal = Proximity Float
            | Altitude Float
            | Axis { pitch :: Float,
                     roll :: Float,
                     yaw :: Float
                   }
            deriving(Show)

type Latitude = Float
type Longitude = Float
type Elevation = Float
type Location = (Latitude,Longitude,Elevation)

type Domain = (Location,OpMode) -> (Location,OpMode)

move :: Message -> Location -> Location
move msg (lat,lng,ele) = case msg of
  Up          f -> (lat,lng,(ele + f))
  Down        f -> (lat,lng,(ele - f))
  Forward     f -> ((lat + f),lng,ele)
  Backward    f -> ((lat - f),lng,ele)
  StrafeLeft  f -> (lat,(lng - f),ele)
  StrafeRight f -> (lat,(lng + f),ele)
  _             -> (lat,lng,ele)

status :: Signal -> Location -> OpMode -> OpMode
status = undefined
