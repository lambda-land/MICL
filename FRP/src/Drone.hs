module Drone where

import Reactive.Banana
import Reactive.Banana.Frameworks


type EventHandle a = (AddHandler a, Handler a)

event :: EventHandle a -> MomentIO (Event a)
event = fromAddHandler . fst

fire :: EventHandle a -> Handler a
fire = snd
  

data World = World {
  time    :: Behavior Integer
}

-- | A quadcopter drone is controlled by four rotors:
--    * rotors 1 and 3 rotate clockwise and adjust the pitch
--    * rotors 2 and 4 rotate counterclockwise and adjust the roll
data Drone = Drone {
  rotor1 :: Behavior Float,
  rotor2 :: Behavior Float,
  rotor3 :: Behavior Float,
  rotor4 :: Behavior Float
}

-- | Drone sensors.
data Sensors = Sensors {
  battery :: Behavior Int,
  posX    :: Behavior Float,
  posY    :: Behavior Float,
  posZ    :: Behavior Float,
  pitch   :: Behavior Float,
  roll    :: Behavior Float,
  yaw     :: Behavior Float
}

-- | Negotiation primitives.
data Control = Give | Offer | Request | Take
  deriving (Eq,Show)

type Name = String

data Unit = Unit {
  name :: Name,
  run  :: Event Control
}

whenSensor :: Behavior a -> (a -> Bool) -> Event b -> Event b
whenSensor s f = whenE (fmap f s)

lowBattery :: World -> Sensors -> EventHandle Control -> Unit
lowBattery w ss ctrl = Unit "Low Battery Monitor" $
   whenSensor (battery ss) (<= 2) (fire ctrl Take)
