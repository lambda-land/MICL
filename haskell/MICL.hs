{- | Mixed-Initiative Controller Language (Keeley Abbott -- 2016)
     Description: A programmer is going to give the device or vehicle a series
        of commands to execute in order to navigate a route. The programmer is
        also going to provide a set of protocols for the human actor that
        describe goal-directed tasks for the human actor to complete as well as
        how to recover from some potential exceptions
-}

module MICL where

{- | a message is an output to the drone or vehicle (or in the case of a
         Protocol message to the user) from the program.
     up/down: a value v = { e \forall e \in \mathbb{R} } in meters
     clock/counter: a value v = { c | -180.0 \lt c \lt 180.0 } in degrees
     forward/backward: a value v = { f \forall f \in \mathbb{R} } in meters
     strafe left/right: a value v = { s \forall s \in \mathbb{R} } in meters
     turn left/right: a value v = { t | -180.0 \lt t \lt 180.0 } in degrees
     protocol: a string with goal-directed task(s) for the user to complete, or
         additional information needed to complete the program
-}
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

type OpMode = (Focus,Flag)

data Focus = Human
           | System
           deriving(Show)

data Flag = Exception Flag
          | Recovery
          | Nominal
          | Return
          | Wait
          deriving(Show)


{- | a signal is an input to the drone or vehicle from the environment, the
         program, or the user. proximity is based on location "in front" (in the
         current path) of the device or vehicle.
     proximity: a value v = { p | p \geq 0.0 } in meters (environment)
     altitude: a value v = { a | a \geq 0.0 } in meters (environment)
     axis: a value v = { x | -180.0 \lt x \lt 180.0 } in degrees (environment)
     command: input received via the radio transmitter or some other connection
         to an external source. (user or program)
-}
data Signal = Proximity Float
            | Sensor { pitch :: Float,
                       roll :: Float,
                       yaw :: Float
                     }
            | Command Source
            deriving(Show)

data Source = User
            | Program
            deriving(Show)


{- | domain
-}
type Latitude = Float
type Longitude = Float
type Elevation = Float
type Location = (Latitude,Longitude,Elevation)

type Interaction = (Location,OpMode) -> (Location,OpMode)


{- | combinator functions
-}
instruction :: Message -> Location -> Location
instruction msg (lat,lng,ele) = case msg of
  Up          f -> (lat,lng,(ele + f))
  Down        f -> (lat,lng,(ele - f))
  Forward     f -> ((lat + f),lng,ele)
  Backward    f -> ((lat - f),lng,ele)
  StrafeLeft  f -> (lat,(lng - f),ele)
  StrafeRight f -> (lat,(lng + f),ele)
  _             -> (lat,lng,ele)

status :: Signal -> OpMode -> OpMode
status sig (foc,flg) = case sig of
  Proximity f -> if f <= 0.1
                 then
                   (Human,Exception Wait)
                 else (foc,flg)
  Sensor p r y -> if p <= -15.0 || p >= 15.0
                  then
                    (Human,Exception Wait)
                  else
                    if y <= -15.0 || y >= 15.0
                    then
                      (Human,Exception Wait)
                    else (foc,flg)
  Command s -> case s of
    User -> case (foc,flg) of
      (System,_)                  -> (Human ,flg)
      (Human ,Exception Recovery) -> (Human ,Nominal)
      (Human ,Exception Wait)     -> (Human ,Exception Recovery)
      (Human ,Return)             -> (System,Wait)
      (Human ,Wait)               -> (Human ,Nominal)
      _                           -> (foc   ,flg)
    Program -> case (foc,flg) of
      (Human ,_)                  -> (Human ,Exception Wait)
      (System,Exception Recovery) -> (System,Nominal)
      (System,Exception Wait)     -> (System,Exception Recovery)
      (System,Return)             -> (Human ,Wait)
      (System,Wait)               -> (System,Nominal)
      _                           -> (foc   ,flg)
