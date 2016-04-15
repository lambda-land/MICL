module Main where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)

import System.Console.Haskeline (InputT
                                , defaultSettings
                                , getInputLine
                                , runInputT
                                , setComplete
                                )
import System.Directory (copyFile
                        , createDirectoryIfMissing
                        , doesFileExist
                        , getAppUserDataDirectory
                        )
import System.Environment (getArgs)

import Display
import Examples
import MICL

main :: IO ()
main = do
        args <- getArgs

        if null args
            then do
                putStrLn "Mixed-Initiative Controller Language (MICL) - Interactive Mode"
                runInputT haskelineSettings interactive
            else dispatch args
    where
        haskelineSettings = setComplete autoComplete defaultSettings
