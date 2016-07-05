module ReactiveMICL where

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
--

-- | REACTIONS
--   when the drone encounters an expected event, there is a programmatic
--   way of processing that event, which can be thought of as a
--   programmed reaction. when the drone encounters an unexpected event,
--   the user should be notified and (potentially) prompted with a
--   solution.
--   similarly, when a human actor encoounters an expected event, there
--   is no need to "interrupt" the user, however, if the user encounters
--   an unexpected event there may be need for the user to prompt the
--   program or the drone to provide additional advice or instruction.
--

data Action a = IA a Instruction
              | MA a
              | SA a [Action a]
              deriving Show

type Instruction = String

data Reaction a = IR a Instruction
                | MR a
                | SR a [Reaction a]
                deriving Show

data Signal a = Signal a { channel :: Agent
                         , enable :: Bool
                         , roll :: Double
                         , pitch :: Double
                         , gaz :: Double
                         , yaw :: Double
                         }
              deriving Show

data Agent = Human
           | Computer
           deriving Show

data Flag = Exception
          | NoException
          deriving Show
