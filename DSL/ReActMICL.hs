module ReActMICL where

-- | so, to focus more on the interactive side of this, and really
--   make interactions first-class citizens, I want to focus on the
--   base components of interactions. to that end, there should be
--   actions and reactions, however the lines seem a little
--   blurry when it comes to distinguishing things like whether or
--   not a movement is an action or a reaction to something. maybe
--   this is the best place to start...
--

-- | ACTIONS
--   so, there are going to be *actions* that are code written by the
--   programmer. these actions include moving the drone, and providing
--   instructions to the user for consumption (these instructions might
--   include waypoints that need to be visited, or other useful
--   information of the user).
--   there will also be actions initiated by the user that might
--   interrupt the course of actions being taken by the programmer, but
--   may also enhance those actions.
--

-- | REACTIONS
--   when the drone encounters an expected event, there is a programmatic
--   way of processing that event, which can be thought of as a
--   programmed reaction. when the drone encounters an unexpected event,
--   the user should be notified and (potentially) prompted with a
--   solution.
--   similarly, when a human actor encounters an expected event, there
--   is no need to "interrupt" the user, however, if the user encounters
--   an unexpected event there may be need for the user to prompt the
--   program or the drone to provide additional advice or instruction.
--

data Prog a = MoveA a
            | PromptA a Task
            | SeqA a [Prog a]
            deriving Show

data React a = MoveR a
             | Exception a OpMode
             | PromptR a Task
             | SeqR a [React a]
             deriving Show

type Task = Either Waypoint Instruction

type Waypoint = Location
type Location = (North,East,Down)
type North = Double
type East = Double
type Down = Double

type Instruction = String

type OpMode = (Agent,Mode)

data Agent = Human
           | Computer
           deriving Show

data Mode = Recovery
          | Normal
          | Return
          | Wait
          deriving Show

data Signal = Signal { order :: Int,
                       agent :: Agent,
                       enable :: Bool,
                       roll :: Float,
                       pitch :: Float,
                       gaz :: Float,
                       yaw :: Float,
                       instruction :: String
                     }
            deriving Show

a1 = MoveA Signal { order = 1
                  , agent = Computer
                  , enable = True
                  , roll = 0
                  , pitch = 0
                  , gaz = 1
                  , yaw = 0
                  , instruction = "" }
