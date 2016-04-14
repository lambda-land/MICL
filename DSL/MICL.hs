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

type Domain = Signal -> State

type State = (Servos,Display,Status)


-- | floating point values indicate the amount of voltage (power) being
--       transferred to the servos spinning the props.
--   sequence: is an integer value representing the command's location in the
--       sequence of all other commands
--   channel: interprets how the drone receives commands (either from the user
--       or the computer agent)
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
data Signal = Signal { sequence :: Int,
                       channel :: Agent,
                       flag :: Int32,
                       roll :: Float,
                       pitch :: Float,
                       gaz :: Float,
                       yaw :: Float }
            deriving(Show)

type Servos = (Float,Float,Float,Float)

type Display = String

type Status = (Agent,Mode)

data Agent = Human
           | Computer
           deriving(Show)

data Mode = Recovery
          | Normal
          | Return
          | Wait
          deriving(Show)


-- | movement functions
--   take a signal and apply the appropriate servo changes (the range of input
--       from the signal is -1.0 to 1.0).
--
moveLat :: Signal -> Servos -> Servos
moveLat sig srv
  | (roll sig) < 0 = mapLeftSignal (flip (-) (roll sig)) srv
  | (roll sig) > 0 = mapRghtSignal (+ (roll sig)) srv
  | otherwise      = srv

moveLong :: Signal -> Servos -> Servos
moveLong sig srv
  | (pitch sig) < 0 = mapFrntSignal (flip (-) (pitch sig)) srv
  | (pitch sig) > 0 = mapBackSignal (+ (pitch sig)) srv
  | otherwise     = srv

moveElev :: Signal -> Servos -> Servos
moveElev sig srv
  | (gaz sig) < 0 = mapSignal (flip (-) (gaz sig)) srv
  | (gaz sig) > 0 = mapSignal (+ (gaz sig)) srv
  | otherwise     = srv


-- | status functions
--   switchAgent: takes a signal and a status and updates the agent of the drone
--       based on the channel
--   switchMode: takes a status and updates it based on the current mode of the
--       drone
--
switchAgent :: Signal -> Agent
switchAgent sig = case (channel sig) of
  Computer -> Human
  Human    -> Computer

switchMode :: Status -> Mode
switchMode sts = case (snd sts) of
  Recovery -> Normal
  Return   -> Normal
  Wait     -> Recovery
  _        -> (snd sts)


-- | combinator functions
--   status: takes a signal and a status and updates the status
--
status :: Signal -> Status -> Status
status sig sts = case (snd sts) of
  Recovery -> (fst sts,switchMode sts)
  Return -> case (channel sig) of
    Computer -> (switchAgent sig,switchMode sts)
    _        -> (switchAgent sig,switchMode sts)
  Wait     -> (fst sts,switchMode sts)
  _        -> sts

movement :: Signal -> Servos -> Servos
movement sig srv
  | (roll sig)  /= 0 = moveLat  sig srv
  | (pitch sig) /= 0 = moveLong sig srv
  | (gaz sig)   /= 0 = moveElev sig srv

movements :: [Signal] -> Servos -> Servos
movements []     srv = srv
movements (s:ss) srv = movement s (movements ss srv)

task :: Signal -> Display -> Display
task = undefined

interaction :: Signal -> Servos -> Display -> Status -> (Servos,Display,Status)
interaction sig srv dis sts = (movement sig srv,task sig dis,status sig sts)


-- | mapping function to apply changes to the correct servos based on a signal.
--   mapSignal: increases voltage (power) to all servos
--   mapFrntSignal: increases voltage (power) to the left rear and right rear
--       servos
--   mapBackSignal: increases voltage (power) to the left front and right front
--       servos
--   mapLeftSignal: increases voltage (power) to the right front and right rear
--       servos
--   mapRghtSignal: increases voltage (power) to the left front and left rear
--       servos
--
mapSignal :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
mapSignal f (lf,rf,lr,rr) = (f lf,f rf,f lr,f rr)

mapFrntSignal :: (a -> b) -> (a,a,a,a) -> (a,a,b,b)
mapFrntSignal f (lf,rf,lr,rr) = (lf,rf,f lr,f rr)

mapBackSignal :: (a -> b) -> (a,a,a,a) -> (b,b,a,a)
mapBackSignal f (lf,rf,lr,rr) = (f lf,f rf,lr,rr)

mapLeftSignal :: (a -> b) -> (a,a,a,a) -> (a,b,a,b)
mapLeftSignal f (lf,rf,lr,rr) = (lf,f rf,lr,f rr)

mapRghtSignal :: (a -> b) -> (a,a,a,a) -> (b,a,b,a)
mapRghtSignal f (lf,rf,lr,rr) = (f lf,rf,f lr,rr)


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
