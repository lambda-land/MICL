{-# LANGUAGE ScopedTypeVariables #-}
-- allows pattern signatures like
-- do
--     (b :: Behavior Int) <- stepper 0 ...

{-# LANGUAGE RecursiveDo #-}
-- allows recursive do notation
-- mdo
--     ...

module Interactive where

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

import MICL
import MonadicMICL

main :: IO ()
main = do
  displaySplashScreen
  sources <- makeSources
  network <- compile $ networkDescription sources
  actuate network
  interactive sources

-- Create event sources corresponding to  ascend  and  descend
makeSources = (,) <$> newAddHandler <*> newAddHandler

displaySplashScreen :: IO ()
displaySplashScreen = mapM_ putStrLn $
  "------ Interactive Mode ------":
  "":
  "Commands are:":
  "   ascend  - increase height":
  "   descend - decrease height":
  "   exit    - leave the program":
  "":
  []


-- | interactive
--
interactive :: (EventSource (), EventSource ()) -> IO ()
interactive (ascend,descend) = loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      s <- getLine
      case s of
        "ascend"  -> fire ascend ()
        "descend" -> fire descend ()
        "exit"    -> return ()
        _         -> putStrLn $ s ++ " -- unknown command"
      when (s /= "exit") loop

-- | event sources
--
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd


-- | program logic
--
type Elevation = Float

networkDescription :: (EventSource (), EventSource ()) -> MomentIO ()
networkDescription (ascend,descend) = mdo
  initialStdGen <- liftIO $ newStdGen
  eascend  <- fromAddHandler (addHandler ascend)
  edescend <- fromAddHandler (addHandler descend)
  (eheight :: Event Elevation, bheight :: Behavior Elevation) <- mapAccum 0 . fmap (\f x -> (f x,f x)) $ unions $
    [ addHeight    <$ eascend
    , removeHeight <$ edescend
    ]
  let
    addHeight    = (+1)
    removeHeight = subtract 1

    emaydescend :: Event Bool
    emaydescend = (\height _ -> height > 0) <$> bheight <@> edescend

    edoesdescend :: Event ()
    edoesdescend = () <$ filterE id emaydescend

    edenied :: Event ()
    edenied = () <$ filterE not emaydescend

  reactimate $ putStrLn . showHeight <$> eheight
  reactimate $ putStrLn "Not enough height!" <$ edenied

showHeight elevation = "Height: " ++ show elevation
