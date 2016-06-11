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
--     transferred to the servos spinning the props.
--   channel: interprets how the drone receives commands (either from the user
--     or the computer agent)
--   mode: indicates whether the drone is in a normal mode, recovering from some
--     exception, in a wait status, or is ownership of the process is being
--     returned to the human actor or the computer.
--   enable: interprets how the drone interprets the progressive commands from
--     the user or the program:
--       * bit 0: enable/disable progressive commands
--       * bit 1: enable/disable combined yaw
--   roll: drone left/right tilt value v = { r | -1.0 \leq r \leq 1.0 }
--       * a negative value will make the drone tilt left, thus move left
--       * a positive value will make the drone tilt right, thus move right
--   pitch: drone front/back tilt value v = { p | -1.0 \leq p \leq 1.0 }
--       * a negative value will make the drone lower it's front, thus move
--         forward
--       * a negative value will make the drone raise it's front, thus move
--         backward
--   gaz: drone vertical speed value v = { g | -1.0 \leq g \leq 1.0 }
--       * a negative value will decrease power to all engines, thus move
--         down
--       * a positive value will increase power to all engines, thus move up
--   yaw: drone angular speed value v = { y | -1.0 \leq y \leq 1.0 }
--       * a negative value will make the drone spin (turn) left
--       * a positive value will make the drone spin (turn) right
--
data Input = Input { channel :: Agent
                   , mode :: Mode
                   , enable :: Bool
                   , roll :: Float
                   , pitch :: Float
                   , gaz :: Float
                   , yaw :: Float
                   }
           deriving(Show)

type Output = Task

type Task = Either Waypoint Instruction

type Waypoint = Location

type Location = (North,East,Down)
type North = Float
type East = Float
type Down = Float

type Instruction = String

data Agent = Human
           | Computer
           deriving(Eq,Show)

data Mode = Recovery
          | Normal
          | Return
          | Wait
          deriving(Eq,Show)

type OpMode = (Agent,Mode)


-- | location functions (calculated as a relative location using
--     dead-reckoning.)
--   take a signal and a speed, and produce a double that represents the new
--     relative location of the device or vehicle (reported in meters per
--     second).
--   locateNorth: calculates the new location north of the "home" point based on
--     speed and previous location.
--   locateEast: calculates the new location east of the "home" point based on
--     speed and previous location.
--   locateDown: calculates the new location aboce the "home" point based on
--     speed and previous location.
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
--   locate: takes a signal and a location, and calculates the drone's new
--     location based on the max speed and the signal input.
--   changeAgent: takes a signal and switches the agent of the drone based on
--     the channel.
--   changeMode: takes a signal and switches the mode of the drone based on the current
--     mode.
--   addTask: takes a task and adds it to the current list of tasks.
--   deleteTask: removes a task from the current list of tasks if it exists.
--   clearWaypoint: clears a waypoint from the list of tasks if it exists.
--
locate :: Input -> Location -> Location
locate inp loc = (locateNorth ((pitch inp) * rate)
                  (locateEast ((roll inp) * rate)
                   (locateDown ((gaz inp) * rate) loc)))

rate :: Float
rate = 2.0

changeAgent :: OpMode -> Agent
changeAgent opm = case (fst opm) of
  Computer -> Human
  Human    -> Computer

changeMode :: OpMode -> Mode
changeMode opm = case (snd opm) of
  Recovery -> Normal
  Normal   -> Wait
  Return   -> Wait
  Wait     -> Normal

changeEnable :: Input -> Input
changeEnable inp = case (enable inp) of
  True -> inp { enable = False }
  _    -> inp { enable = True }

addTask :: Output -> Display -> Display
addTask tsk ds = [tsk] ++ ds


deleteTask :: Output -> Display -> Display
deleteTask _   []     = []
deleteTask tsk (d:ds) = case tsk of
  (Left loc) -> case d of
    (Left loc')
      | loc       == loc' -> ds
      | otherwise         -> d : deleteTask tsk ds
  (Right ins) -> case d of
    (Right ins')
      | ins       == ins' -> ds
      | otherwise         -> d : deleteTask tsk ds

clearWaypoint :: Location -> Display -> Display
clearWaypoint loc [] = []
clearWaypoint loc (d:ds) = case d of
  (Left loc')
    | loc       == loc' -> ds
    | otherwise         -> d : clearWaypoint loc ds


-- | semantic domain
--
type Domain s = s -> Status -> Status

type Status = (Location,Display,OpMode)


-- | display type synonyms
--
type Display = [Task]
