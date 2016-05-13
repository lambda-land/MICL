module Main where

import Control.Monad.Trans (liftIO)
import Control.Exception (catch)
import qualified Control.Exception as E
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
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
cmds =
  [("add",    cmdAdd,    "<description>",     "adds a task")
  ,("change", cmdChange, "<id><description>", "changes a task")
  ,("rm",     cmdRm,     "<id>",              "removes a task")
  ,("done",   cmdDone,   "<id>",              "marks a task as done")
  ,("undone", cmdUndone, "<id>",              "resets the done state for a task")
  ,("list",   cmdList,   "[tag | -tag ...]",  "list all tasks with optional filtering")
  ,("help",   cmdHelp,   "",                  "help information")
  ]

cmdAdd :: CmdHandler
cmdAdd []   = printUsage "add"
cmdAdd desc = modifyDB (add $ unwords desc)

cmdChange :: CmdHandler
cmdChange (idStr:desc)
  | not (null desc) = modifyDB (changeTask (read idStr) (const $ unwords desc))
cmdChange _         = printUsage "change"

cmdRm :: CmdHandler
cmdRm (idStr:[]) = modifyDB (delete $ read idStr)
cmdRm _          = printUsage "rm"

cmdDone :: CmdHandler
cmdDone (idStr:[]) = modifyDB (adjustTask (read idStr) (addTag $ "@done{" ++ date ++ "}"))
cmdDone _          = printUsage "done"

cmdUndone :: CmdHandler
cmdUndone (idStr:[]) = modifyDB (adjustTask (read idStr) (deleteTag "@done"))
cmdUndone _          = printUsage "undone"

cmdList :: CmdHandler
cmdList tags = withDB (list tagFilter) >>= putStr
    where
        defaultFilter = ["-@done"]
        tagFilter = foldl' updateFilter (Set.empty, Set.empty) (defaultFilter ++ tags)
        updateFilter (sel, desel) ('-':tag) = (Set.delete tag sel, Set.insert tag desel)
        updateFilter (sel, desel) ('+':tag) = (sel, Set.delete tag desel)
        updateFilter (sel, desel) tag       = (Set.insert tag sel, Set.delete tag desel)

cmdHelp :: CmdHandler
cmdHelp _ = do
        putStrLn "Task Commands:"
        putStr . unlines . map printCmdInfo $ cmds



-- | monadic operations
--
withDB :: (TodoDB -> a) -> IO a
withDB f = fmap f load

modifyDB :: (TaskDB -> TaskDB) -> IO ()
modifyDB f = catch doModify handleError
    where
      doModify = do
        db' <- withDB f
        db' `seq` save db'
      handleError :: E.SomeException -> IO ()
      handleError e = print e >> return ()


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
