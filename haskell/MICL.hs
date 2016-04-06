module MICL where

type Euclidean = (Int, Int, Int)

data Sensor = Accelerometer Euclidean
            | Gravity Euclidean
            | Gyroscope Euclidean
            | LinearAccel Euclidean
            | Orientation Euclidean
            | Proximity Euclidean
            | RotationVect Euclidean
            | Status [Sensor]
            deriving(Eq,Show)

type User = String

data OpMode = Exception OpMode
            | Recovery User
            | Nominal User
            | Wait
            deriving(Eq,Show)

type Degrees = Int

data CamMode = VideoOn
             | VideoOff
             | Capture
             deriving(Eq,Show)

data ZoomTy = Optical Bool
            | Digital Bool
            deriving(Eq,Show)

data Cmd = Elevation Degrees
         | Heading Degrees
         | Bank Degrees
         | Camera CamMode
         | CamZoom ZoomTy Degrees
         | CamPan Degrees
         | CamTilt Degrees
         | Seq [Cmd]
         deriving(Eq,Show)

type Domain = (Sensor,OpMode) -> (Cmd,OpMode)
