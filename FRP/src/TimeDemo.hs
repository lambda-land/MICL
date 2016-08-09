module TimeDemo where

import System.IO (hSetEcho,stdin)

import Data.Time.Clock

import Reactive.Banana
import Reactive.Banana.Frameworks


type EventHandle a = (AddHandler a, Handler a)

event :: EventHandle a -> MomentIO (Event a)
event = fromAddHandler . fst

fire :: EventHandle a -> Handler a
fire = snd

data Handles = Handles {
  key :: EventHandle Char
}

-- | Time elapsed since the given argument in milliseconds.
elapsedTime :: UTCTime -> IO Integer
elapsedTime start = do
  now <- getCurrentTime
  let (secs,frac) = properFraction (diffUTCTime now start)
  return (1000 * secs + round (1000 * frac))

eventHandles :: IO Handles
eventHandles = do
  key <- newAddHandler
  return (Handles key)

buildNetwork :: Handles -> MomentIO ()
buildNetwork hs = do
  start <- liftIO getCurrentTime
  bTime <- fromPoll (elapsedTime start)
  eKey  <- event (key hs)
  let eTime = bTime <@ eKey
  reactimate (fmap print eTime)

eventLoop :: Handles -> IO ()
eventLoop hs = hSetEcho stdin False >> loop
  where
    loop = liftIO getChar >>= fire (key hs) >> loop

main :: IO ()
main = do
  hs <- eventHandles
  network <- compile (buildNetwork hs)
  actuate network
  eventLoop hs
