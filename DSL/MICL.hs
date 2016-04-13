{- | Mixed-Initiative Controller Language (Keeley Abbott -- 2016)
     Description: A programmer is going to give the device or vehicle a series
        of commands to execute in order to navigate a route. The programmer is
        also going to provide a set of protocols for the human agent that
        describe goal-directed tasks for the human agent to complete as well as
        some additional information, and potentially how to recover from some
        exceptions.
-}

module MICL where

import Data.Int
import System.IO

type Domain = Signal -> State

type State = (Servos,Display,Status)


-- | floating point values are most commonly given in 0.25 increments to
--       indicate the amount of power being sent to each of the engines.
--   sequence: is an integer value representing the command's location in the
--       sequence of all other commands
--   channel: interprets how the drone receives commands (either from the user
--       or the computer):
--           * a value of false indicates a human agent
--           * a value of true indicates a computer agent
--   flag: interprets how the drone interprets the progressive commands from the
--       user or the program:
--           * bit 0: enable/disable progressive commands
--                    if set to zero will put the drone in a "hover" state
--                    if set to one will make the drone consider the values of
--                        progressive commands
--           * bit 1: enable/disable combined yaw
--                    if set to zero will consider yaw arguments passed
--                    if set to one will ignore yaw arguments (for base turns)
--           * bit 2-31: are not used, so are set to 0
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
data Signal = Signal { index :: Int,
                       channel :: Bool,
                       flag :: Int32,
                       roll :: Float,
                       pitch :: Float,
                       gaz :: Float,
                       yaw :: Float }

type Servos = (Float,Float,Float,Float)

type Display = IO ()

type Status = (Agent,Mode)

data Agent = Human
           | Computer

data Mode = Recovery
          | Normal
          | Return
          | Wait


-- | servo functions that take a signal and apply the appropriate servo
--       changes (the range of input from the signal is -1.0 to 1.0 in
--       0.25 increments representing 1/4 powers of engine speed).
--
moveLeft :: Signal -> Servos -> Servos
moveLeft sig srv = mapLeftSignal (flip (-) (get_r sig)) srv

moveRght :: Signal -> Servos -> Servos
moveRght sig srv = mapRghtSignal (+ (get_r sig)) srv

moveFwd :: Signal -> Servos -> Servos
moveFwd sig srv = mapFrntSignal (flip (-) (get_p sig)) srv

moveBack :: Signal -> Servos -> Servos
moveBack sig srv = mapBackSignal (+ (get_p sig)) srv

moveUp :: Signal -> Servos -> Servos
moveUp sig srv = mapSignal (+ (get_g sig)) srv

moveDown :: Signal -> Servos -> Servos
moveDown sig srv = mapSignal (flip (-) (get_g sig)) srv


-- | status functions
--   switchAgent: takes a signal and a status and updates the agent of the drone
--       based on the channel
--   switchMode: takes a status and updates it based on the current mode of the
--       drone
--
switchAgent :: Signal -> Agent
switchAgent sig = case (get_c sig) of
  True -> Computer
  _    -> Human

switchMode :: Status -> Mode
switchMode sts = case (snd sts) of
  Recovery -> Normal
  Normal   -> Wait
  Return   -> Normal
  Wait     -> Recovery


-- | combinator functions
--   status: takes a signal and a status and updates the status
--
status :: Signal -> Status -> Status
status sig sts = case (snd sts) of
  Return -> case (get_c sig) of
    True -> (Human   ,switchMode sts)
    _    -> (Computer,switchMode sts)
  _      -> (switchAgent sig,switchMode sts)

movement :: Signal -> Servos -> Servos
movement sig srv
  | (get_r sig) < 0 = moveLeft sig srv
  | (get_r sig) > 0 = moveRght sig srv
  | (get_p sig) < 0 = moveFwd  sig srv
  | (get_p sig) > 0 = moveBack sig srv
  | (get_g sig) < 0 = moveDown sig srv
  | (get_g sig) > 0 = moveUp   sig srv
  | otherwise       = srv

task :: Signal -> Display -> Display
task = undefined

interaction :: Signal -> Servos -> Display -> Status -> (Servos,Display,Status)
interaction sig srv dis sts = (movement sig srv,task sig dis,status sig sts)


-- | record accessing helper functions for the signals
--
get_i :: Signal -> Int
get_i = index

get_c :: Signal -> Bool
get_c = channel

get_f :: Signal -> Int32
get_f = flag

get_r :: Signal -> Float
get_r = roll

get_p :: Signal -> Float
get_p = pitch

get_g :: Signal -> Float
get_g = gaz

get_y :: Signal -> Float
get_y = yaw


-- | mapping function to apply changes to all servos based on a signal
--
mapSignal :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
mapSignal f (a1,a2,a3,a4) = (f a1,f a2,f a3,f a4)

mapFrntSignal :: (a -> b) -> (a,a,a,a) -> (b,b,a,a)
mapFrntSignal f (a1,a2,a3,a4) = (f a1,f a2,a3,a4)

mapBackSignal :: (a -> b) -> (a,a,a,a) -> (a,a,b,b)
mapBackSignal f (a1,a2,a3,a4) = (a1,a2,f a3,f a4)

mapLeftSignal :: (a -> b) -> (a,a,a,a) -> (b,a,b,a)
mapLeftSignal f (a1,a2,a3,a4) = (f a1,a2,f a3,a4)

mapRghtSignal :: (a -> b) -> (a,a,a,a) -> (a,b,a,b)
mapRghtSignal f (a1,a2,a3,a4) = (a1,f a2,a3,f a4)


-- | accessing individual servos
--
fstServ :: Servos -> Float
fstServ (a,b,c,d) = a

sndServ :: Servos -> Float
sndServ (a,b,c,d) = b

thrdServ :: Servos -> Float
thrdServ (a,b,c,d) = c

frthServ :: Servos -> Float
frthServ (a,b,c,d) = d
