{-# LANGUAGE ScopedTypeVariables #-}
-- allows pattern signatures like
-- do
--     (b :: Behavior Int) <- stepper 0 ...

{-# LANGUAGE RecursiveDo #-}
-- allows recursive do notation
-- mdo
--     ...

module Interactive where

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX
import System.Random


-- | main
--
height, width, depth :: Int
height = 300
width  = 300
depth  = 300

ship :: Bitmap ()
ship = bitmap "ship.ico"

main :: IO ()
main = start flightsim


-- | flight logic
--
flightsim :: IO ()
flightsim = do
  ff <- frame [ text       := "Flight Sim"
              , bgcolor    := white
              , resizeable := False ]

  status <- statusField [ text := "Welcome to MICLs Flight Simulator" ]
  set ff [ statusBar := [status] ]

  t <- timer ff [ interval := 50 ]

  flight <- menuPane        [ text := "&Flight" ]
  new    <- menuItem flight [ text := "&New\tCtrl+N", help := "New flight" ]
  pause  <- menuItem flight [ text      := "&Pause\tCtrl+P"
                            , help      := "Pause flight"
                            , checkable := True
                            ]
  menuLine flight
  quit <- menuQuit flight [ help := "Quit the flight" ]

  set new   [ on command := flightsim ]
  set pause [ on command := set t [ enabled :~ not ] ]
  set quit  [ on command := close ff ]

  set ff [ menuBar := [ flight ] ]

  pp <- panel ff []
  set ff [ layout := minsize (sz width height) $ widget pp ]
  set pp [ on (charKey '-') := set t [ interval :~ \i -> i * 2 ]
         , on (charKey '+') := set t [ interval :~ \i -> max 10 (div i 2) ]
         ]

  -- event network
  let networkDescription :: MomentIO ()
      networkDescription = mdo
        -- timer
        etick <- event0 t command

        -- keyboard events
        ekey <- event1 pp keyboard
        let eup  = filterE ((== KeyUp      ) . keyKey) ekey
            edwn = filterE ((== KeyDown    ) . keyKey) ekey
            efwd = filterE ((== charKey 'w') . keyKey) ekey
            ebwd = filterE ((== charKey 's') . keyKey) ekey
            elft = filterE ((== charKey 'a') . keyKey) ekey
            ergt = filterE ((== charKey 'd') . keyKey) ekey

        -- ship position
        (bship :: Behavior Int)
          <- accumB (width `div` 2) $ unions
          [ goLeft  <$ elft
          , goRight <$ ergt
          , goFwd   <$ efwd
          , goBack  <$ ebwd
          , goUp    <$ eup
          , goDown  <$ edwn
          ]

        let
          goLeft  x = max 0            (x - 5)
          goRight x = min (width - 30) (x + 5)
          goFwd   y = min (width - 30) (y + 5)
          goBack  y = max 0            (y - 5)
          goUp    z = min (width - 30) (z + 5)
          goDown  z = max 0            (z - 5)

        -- draw the flight state
        bpaint <- stepper (\_dc_ -> return ()) $
          (drawFlightState <$> bship) <@ etick
        sink pp [ on paint :== bpaint ]
        reactimate $ repaint pp <$ etick

  network <- compile networkDescription
  actuate network


-- draw flight state
drawFlightState :: Int -> DC a -> b -> IO ()
drawFlightState ship dc _view = do
  let
    shipLocation = point ship (width `div` 2)

  drawShip dc shipLocation

drawShip :: DC a -> Point -> IO ()
drawShip dc pos = drawBitmap dc ship pos True []
