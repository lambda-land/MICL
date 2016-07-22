module Demo where

import Control.Monad (when)
import System.IO (hSetEcho,stdin)

import Reactive.Banana
import Reactive.Banana.Frameworks


-- 
-- * Model
--

-- | A robot powered by two wheels that move independently.
type Robot = (Int,Int)

faster :: Robot -> Robot
faster (l,r) = (l+1,r+1)

slower :: Robot -> Robot
slower (l,r) = (l-1,r-1)

lefter :: Robot -> Robot
lefter (l,r) = (l-1,r+1)

righter :: Robot -> Robot
righter (l,r) = (l+1,r-1)

stop :: Robot -> Robot
stop _ = (0,0)


--
-- * Control
--

type KeyEvent a = (AddHandler a, Handler a)

event :: KeyEvent a -> MomentIO (Event a)
event = fromAddHandler . fst

fire :: KeyEvent a -> Handler a
fire = snd

data Keys = Keys {
  up    :: KeyEvent (),
  down  :: KeyEvent (),
  left  :: KeyEvent (),
  right :: KeyEvent ()
}

keyEvents :: IO Keys
keyEvents = do
  up    <- newAddHandler
  down  <- newAddHandler
  left  <- newAddHandler
  right <- newAddHandler
  return (Keys up down left right)

buildNetwork :: Keys -> MomentIO ()
buildNetwork keys = do
  eUp    <- event (up keys)
  eDown  <- event (down keys)
  eLeft  <- event (left keys)
  eRight <- event (right keys)
  robot  <- accumE (0,0) $ unions [
    faster  <$ eUp,
    slower  <$ eDown,
    lefter  <$ eLeft,
    righter <$ eRight]
  reactimate (fmap print robot)

eventLoop :: Keys -> IO ()
eventLoop keys = hSetEcho stdin False >> loop
  where
    loop = do
      c <- liftIO getChar
      case c of
        'w' -> fire (up keys) ()
        'a' -> fire (left keys) ()
        's' -> fire (down keys) ()
        'd' -> fire (right keys) ()
        _   -> return ()
      when (c /= 'q') loop

main :: IO ()
main = do
  keys <- keyEvents
  network <- compile (buildNetwork keys)
  actuate network
  eventLoop keys
