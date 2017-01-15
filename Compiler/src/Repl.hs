module Repl where

import qualified Data.Map as M
import System.IO
import Grammar
import Interpreter
import PrettyPrinter
import Data.Text
import System.Console.Haskeline
import qualified Data.Vector as V

mainLoop :: String -> Env -> InputT IO ()
mainLoop lastStr env = do
    Just l <- getInputLine "repl> "
    let s : ss = Prelude.words l
    let prog = Prelude.unwords ss
    case s of
        ":i" -> do
            case Grammar.parse prog of
                Right stmt -> case eval stmt ([], env) of
                    Right (strList, newEnv) -> do
                        outputStr (Prelude.unlines (Prelude.reverse strList))
                        mainLoop prog newEnv
                    Left errStr -> do
                        outputStrLn $ "Error: " ++ errStr
                        mainLoop prog env   
                Left errStr -> do
                    outputStrLn $ "Error: " ++ errStr
                    mainLoop prog env
        ":q" -> do
            outputStrLn "Good Bye~"
            return ()
        ":t" -> case lastStr of
            "" -> do
                outputStrLn $ "There is no previous program lines!"
                mainLoop lastStr env
            _ -> case prettyPrinter [lastStr] of
                Right ansList -> do
                    outputStr (Prelude.unlines ansList)
                    mainLoop lastStr env
                Left errStr -> do
                    outputStrLn $ "Error: " ++ errStr
                    mainLoop lastStr env
        _ -> do
            outputStrLn $ "Invalid command! (valid commands include :i, :q and :t)"
            mainLoop lastStr env 

repl :: IO ()
repl = do
    putStrLn "Welcome to a simple S-Expression REPL!"
    runInputT defaultSettings (mainLoop "" (M.empty))