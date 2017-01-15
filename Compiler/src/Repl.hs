module Repl where

import qualified Data.Map as M
import System.IO
import Grammar
import Interpreter
import Data.Text
import qualified Data.Vector as V

mainLoop :: Env -> IO ()
mainLoop env = do
    putStr "repl> "
    hFlush stdout
    l <- getLine
    case Grammar.parse l of
        Right stmt -> case eval stmt ([], env) of
            Right (strList, newEnv) -> do
                putStr (Prelude.unlines (Prelude.reverse strList))
                mainLoop newEnv
            Left errStr -> do
                putStrLn $ "Error: " ++ errStr
                mainLoop env   
        Left errStr -> do
            putStrLn $ "Error: " ++ errStr
            mainLoop env

repl :: IO ()
repl = do
    putStrLn "Welcome to a simple S-Expression REPL!"
    mainLoop (M.empty)