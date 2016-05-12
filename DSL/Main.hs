module Main where

import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.Console.ANSI (Color(..)
                           , ColorIntensity(..)
                           , ConsoleLayer(..)
                           , SGR(..)
                           , Underlining(..)
                           , clearScreen
                           , setCursorPosition
                           , setSGRCode
                           )
import System.Console.Haskeline (InputT
                                , defaultSettings
                                , getInputLine
                                , runInputT
                                , setComplete
                                )
import System.Console.Haskeline.Completion (CompletionFunc, simpleCompletion)
import System.Environment (getArgs)
import System.IO (IOMode(..), hGetContents, hPutStr, withFile)
import Text.Printf (printf)

import MICL
import MonadicMICL


-- | main
--
main :: IO ()
main = do
        args <- getArgs

        if null args
            then do
                putStrLn "Interactive Mode"
                runInputT haskelineSettings interactive
            else dispatch args
    where
        haskelineSettings = setComplete autoComplete defaultSettings


-- | interactive
--
interactive :: InputT IO ()
interactive = do
    line <- getInputLine "> "
    case line of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "clear" -> liftIO resetScreen >> interactive
        Just input -> liftIO (dispatch (words input)) >> interactive


-- | commands
--
type CmdHandler = [String] -> IO ()
type CmdInfo = (String, CmdHandler, String, String)

dispatch :: [String] -> IO ()
dispatch [] = return ()
dispatch (cmd:args) = case lookupCmd cmd of
        Nothing        -> printError $ "Unknown command: " ++ unwords (cmd:args)
        Just (_,f,_,_) -> f args

cmds :: [CmdInfo]
cmds = undefined


-- | auto complete
--
autoComplete :: CompletionFunc IO
autoComplete (leftStr, _)
        | length (words leftStr) > 1 = return ("", [])
        | otherwise = return ("", completions)
    where
        word = reverse leftStr
        completions  = map simpleCompletion $ filter (word `isPrefixOf`) cmdNames
        cmdNames = map cmdInfoToName cmds
        cmdInfoToName (name,_,_,_) = name


-- | helper functions
--
printError :: String -> IO ()
printError x = do
    putStrLn x
    putStrLn "Use 'help' for usage information."

printUsage :: String -> IO ()
printUsage = putStrLn . ("Usage: " ++) . printCmdInfo . fromJust . lookupCmd

lookupCmd :: String -> Maybe CmdInfo
lookupCmd cmd = lookup cmd $ map cmdInfoMap cmds
    where
        cmdInfoMap (name,handler,args,desc) = (name, (name, handler, args, desc))

printCmdInfo :: CmdInfo -> String
printCmdInfo (name, _, args, desc) = printf "%-10s %-20s %s" name args desc

resetScreen :: IO ()
resetScreen = clearScreen >> setCursorPosition 0 0
