module Examples where

import MICL

liftOffHover :: [Signal]
liftOffHover = [(Signal 1 Computer 1 0.0 0.0 1.0 0.0),(Signal 1 Computer 1 0.0 0.0 0.0 0.0)]

initSrv :: Servos
initSrv = (0.0,0.0,0.0,0.0)

initDsply :: Display
initDsply = ["taking off..."]

initSts :: Status
initSts = (Computer,Normal)

takeOff :: State
takeOff = interaction liftOffHover initSrv initDsply initSts
