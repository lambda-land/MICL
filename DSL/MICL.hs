{- | Mixed-Initiative Controller Language (Keeley Abbott -- 2016)
     Description: A programmer is going to give the device or vehicle a series
        of commands to execute in order to navigate a route. The programmer is
        also going to provide a set of protocols for the human agent that
        describe goal-directed tasks for the human agent to complete as well as
        some additional information, and potentially how to recover from some
        exceptions.
-}

module MICL where

{- | a message is an instruction for the drone or vehicle (or in the case of
         a Task it is a message to the user) from the program or the human
         agent.
     up/down: a value v = { e \forall e \in \mathbb{R} } in meters
     forward/backward: a value v = { f \forall f \in \mathbb{R} } in meters
     strafe left/right: a value v = { s \forall s \in \mathbb{R} } in meters
     turn left/right: a value v = { t | -180.0 \lt t \lt 180.0 } in degrees
     task: a string with goal-directed instruction(s) for the user to complete,
         or additional information needed to complete the program
-}
data Message = Up Float
             | Down Float
             | Forward Float
             | Backward Float
             | StrafeLeft Float
             | StrafeRight Float
             | TurnLeft Float
             | TurnRight Float
             | Task Code String
             deriving(Show)


{- | a code is a feature that give some additional information to the human
         actor about completing a task.
     broadening: provides clues to help the human agent know how far they can
         diverge from the protocol steps
     narrowing: calls attention to especially important items for the human
         agent in completing the protocol steps
     recovering: provides clues to help the human agent recover from potential
         errors (possibly caused by straying from a previous protocol step)
-}
data Code = Broadening String
          | Narrowing String
          | Recovering String
          | Empty
          deriving(Show)

type OpMode = (Agent,Maybe Exception)

data Agent = Human
           | System
           deriving(Show)

data Exception = Recovery
               | Nominal
               | Return
               | Wait
               deriving(Show)


{- | a signal is an input to the drone or vehicle from the environment, the
         program, or the user. proximity is based on location in the current
         path of the device or vehicle.
     proximity: a value v = { p | p \geq 0.0 } in meters (environment)
     altitude: a value v = { a | a \geq 0.0 } in meters (environment)
     axis: a value v = { x | -180.0 \lt x \lt 180.0 } in degrees (environment)
     command: input received via the radio transmitter or some other connection
         to an external source. (user or program)
-}
data Signal = Proximity Float
            | Altitude Float
            | Sensor { pitch :: Float,
                       roll :: Float,
                       yaw :: Float
                     }
            | Command Source Message
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
     instruction: takes a signal and a location, and updates the location based
         on the command issued (some messages don't change the location)
     status: takes a signal a location and an operating mode, and updates the
         operating mode based on some parameters of the external environment,
         or an input from the system or the human agent
-}
instruction :: Signal -> Location -> Location
instruction sig (lat,lng,ele) = case sig of
  Altitude f       -> (lat,lng,f)
  Command  src msg -> case msg of
    Up          f -> (lat,lng,(ele + f))
    Down        f -> (lat,lng,(ele - f))
    Forward     f -> ((lat + f),lng,ele)
    Backward    f -> ((lat - f),lng,ele)
    StrafeLeft  f -> (lat,(lng - f),ele)
    StrafeRight f -> (lat,(lng + f),ele)
    _             -> (lat,lng,ele)
  _                -> (lat,lng,ele)

status :: Signal -> Location -> OpMode -> OpMode
status sig (lat,lng,ele) (agt,exc) = case sig of
  Proximity f  -> if f <= 0.1
                  then (Human,Just (Wait))
                  else (agt,exc)
  Altitude f   -> if f <= (0.5 * ele)
                  then (Human,Just (Wait))
                  else (agt,exc)
  Sensor p r y -> if p <= -15.0 || p >= 15.0
                  then (Human,Just (Wait))
                  else (agt,exc)
  Command s m  -> case s of
    User    -> case (agt,exc) of
      (System,_)               -> (Human ,exc)
      (Human ,Just (Recovery)) -> (Human ,Nothing)
      (Human ,Just (Wait))     -> (Human ,Just (Recovery))
      (Human ,Just (Return))   -> (System,Just (Wait))
      _                        -> (agt,exc)
    Program -> case (agt,exc) of
      (Human ,_)               -> (Human ,Just (Wait))
      (System,Just (Recovery)) -> (System,Nothing)
      (System,Just (Wait))     -> (System,Just (Recovery))
      (System,Just (Return))   -> (Human ,Just (Wait))
      _                        -> (agt,exc)
