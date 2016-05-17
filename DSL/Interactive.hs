{-# LANGUAGE ScopedTypeVariables #-}
-- allows pattern signatures like
-- do
--     (b :: Behavior Int) <- stepper 0 ...

{-# LANGUAGE RecursiveDo #-}
-- allows recursive do notation
-- mdo
--     ...

module Interactive where

import Control.Monad
import Control.Monad.State

import System.Process

import Graphics.UI.WX hiding (Event,get)
import Reactive.Banana
import Reactive.Banana.WX

import MICL
import MonadicMICL


-- | main
--
main :: IO ()
main = start $ do
    f        <- frame    [text := "Status Monitor"]
    loc      <- staticText f []
    dis      <- staticText f []
    opm      <- staticText f []

    set f [layout := minsize (sz 250 70) $ margin 10 $
            column 10 [label "Status Information",
                       grid 5 5 [[label "Location: ", widget loc]
                                ,[label "Display: ", widget dis]
                                ,[label "OpMode: ", widget opm]]
                      ]
          ]

    t <- timer f [ interval := 500 ] -- timer every 500 ms

    let networkDescription :: MomentIO ()
        networkDescription = do
            bstatus  <- get
            etick    <- event0 t command

            sink loc [ text :== (fst3 bstatus) ]
            sink dis [ text :== (snd3 bstatus) ]
            sink opm [ text :== (thd3 bstatus) ]

    network <- compile networkDescription
    actuate network
