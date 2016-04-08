module MICL where

type Channel = String

data Signal = In Channel
            | Out Channel
            deriving(Eq,Show)

data Focus = Human
           | System
           | Shared
           deriving(Eq,Show)

data OpMode = Exception OpMode
            | Recovery Focus
            | Nominal Focus
            | Wait Focus
            deriving(Eq,Show)

type Status = (Signal,OpMode)
type Location = (Int,Int,Int)

{- combinator function -}
instruction :: (Location,Status) -> Status
instruction (l,(s,o)) = undefined

{- domain -}
type D = (Location,Status) -> (Location,Status)
