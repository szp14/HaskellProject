module Argument where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State
import System.Environment
import System.IO
import Repl
import AST
import Lexical

data Option = Option {
    inPath :: String,
    outPath :: String, 
    funType :: Int
    -- 0: intepret
    -- 1: AST
    -- 2: REPL
}
    deriving Show

type Parser a = StateT [String] IO a

parseFlag :: String -> Parser String
parseFlag f = do
    args <- get
    case args of
        [] -> empty
        (arg : args')
            | arg == "-" ++ f -> do
                put args'
                return f
            | otherwise -> empty
            
parseField :: String -> Parser String
parseField f = do
    parseFlag f
    case f of
        "repl" -> do
            return ""
        _ -> do
            args <- get
            case args of
                [] -> empty
                (arg : args') -> do
                    put args'
                    return arg
            
parseInPath :: Parser String
parseInPath = parseField "i"

parseTreePath :: Parser String
parseTreePath = parseField "t"

parseOutPath :: Parser String
parseOutPath = parseField "o"

parseRepl :: Parser String
parseRepl = parseField "repl"

parseOption :: Parser Option
parseOption = p0 <|> p1 <|> p2 <|> p3 <|> p4 where
    p0 = do
        i <- parseInPath
        o <- parseOutPath
        return (Option i o 0)
    p1 = do
        i <- parseInPath
        return (Option i "" 0)
    p2 = do
        t <- parseTreePath
        o <- parseOutPath
        return (Option t o 1)
    p3 = do
        t <- parseTreePath
        return (Option t "" 1)
    p4 = do
        r <- parseRepl
        return (Option "" "" 2)

defMain :: IO ()
defMain = do
    args <- getArgs
    (option, _) <- runStateT parseOption args
    case funType option of
        2 -> repl
        _ -> do
            inH <- openFile (inPath option) ReadMode
            readCon <- hGetContents inH
            let ast = parseAST readCon
            case ast of
                Left errstr -> putStrLn errstr
                Right stmt -> do
                    writeCon <- case funType option of
                        0 -> do
                            let (Exp expr) = stmt
                            return (show (eval expr M.empty))
                        1 -> do
                            return ""
                    case outPath option of
                        "" -> do
                            putStr writeCon
                        outFile -> do
                            writeFile outFile writeCon
            hClose inH