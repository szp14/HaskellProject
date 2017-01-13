module Repl where

import qualified Data.Map as M
import System.IO
import Lexical
import AST
import Data.Text



data Value a
    = BoolVal {boolVal :: Bool}
    | NumVal {numVal :: Double}
    | CharVal {charVal :: Char}
    | ListVal {listVal :: [a]}

instance Show a => Show (Value a) where
    show (BoolVal val) = show val
    show (NumVal val) = show val
    show (CharVal val) = show val
    show (ListVal val) = show val

type Env = M.Map String (Value (Value ()))

getBool :: BoolExpr -> Env -> Either String (Value (Value ()))
getBool FalseLit env = Right (BoolVal False)
getBool TrueLit env = Right (BoolVal True)
getBool (Not p) env = do
    BoolVal val <- getBool p env
    return (BoolVal (not val))
getBool (And p q) env = do 
    BoolVal val1 <- getBool p env
    BoolVal val2 <- getBool q env
    return (BoolVal (val1 && val2))
getBool (Or p q) env = do
    BoolVal val1 <- getBool p env
    BoolVal val2 <- getBool q env
    return (BoolVal (val1 || val2))
getBool (BVar v@(TypeBool var)) env = do
    case M.lookup (show v) env of 
        Just (BoolVal val) -> do
            return (BoolVal val)
        Nothing -> do
            Left ("The variable " ++ (show var) ++ " is undefined!")
        Just _ -> do
            Left ("The type of variable " ++ (show var) ++ " should be Bool!")

getNum :: NumExpr -> Env -> Either String (Value (Value ()))
getNum (Number a) env = do
    return (NumVal a)
getNum (Add p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (NumVal (num1 + num2))
getNum (Sub p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (NumVal (num1 - num2))
getNum (Mul p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (NumVal (num1 * num2))
getNum (Div p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (NumVal (num1 / num2))
getNum (E p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (BoolVal (num1 == num2))
getNum (L p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (BoolVal (num1 < num2))
getNum (G p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (BoolVal (num1 > num2))
getNum (LE p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (BoolVal (num1 <= num2))
getNum (GE p q) env = do
    NumVal num1 <- getNum p env
    NumVal num2 <- getNum q env
    return (BoolVal (num1 >= num2))
getNum (NVar v@(TypeNum var)) env = do
    case M.lookup (show v) env of 
        Just (NumVal val) -> do
            return (NumVal val)
        Nothing -> do
            Left ("The variable " ++ (show var) ++ " is undefined!")
        Just _ -> do
            Left ("The type of variable " ++ (show var) ++ " should be Num!")

getList :: ListExpr -> Env -> Either String (Value (Value ()))
getList Nil env = do
    return (ListVal [])
getList (CharLit n) env = do
    return (ListVal [CharVal n])
getList (StringLit n) env = do
    return (ListVal (Prelude.map CharVal n))
getList (Cons p q) env = do
    ListVal lList <- getList p env
    ListVal rList <- getList q env
    return (ListVal (lList ++ rList))
getList (Car (Cons p q)) env = getList p env
getList (Cdr (Cons p q)) env = getList q env
getList (LVar v@(TypeList var)) env = do
    case M.lookup (show v) env of 
        Just (ListVal val) -> do
            return (ListVal val)
        Nothing -> do
            Left ("The variable " ++ (show var) ++ " is undefined!")
        Just _ -> do
            Left ("The type of variable " ++ (show var) ++ " should be List!")

eval :: Expr -> Env -> Either String (Value (Value ()))
eval (EVar var) env = do
    case M.lookup (show var) env of
        Just val -> do
            return val
        Nothing -> do
            Left ("The variable " ++ (show var) ++ " is undefined!")
eval (BExp expr) env = getBool expr env
eval (NExp expr) env = getNum expr env
eval (LExp expr) env = getList expr env

mainLoop :: Env -> IO ()
mainLoop env = do
    putStr "repl> "
    hFlush stdout
    l <- getLine
    case parseAST l of
        Right stmt -> case stmt of
            Exp expr -> case eval expr env of
                Right val -> do
                    putStrLn $ (show val)
                    mainLoop env
                Left errStr -> do
                    putStrLn $ "Error: " ++ errStr
                    mainLoop env
            Assign var expr -> case eval expr env of
                Right val -> do
                    putStrLn $ "variable " ++ (show var) ++ " is set to " ++ (show val) 
                    mainLoop (M.insert (show var) val env)
                Left errStr -> do
                    putStrLn $ "Error: " ++ errStr
                    mainLoop env
        Left errStr -> do
            putStrLn $ "Error: " ++ errStr
            mainLoop env

repl :: IO ()
repl = do
    putStrLn "This is a simple REPL. Be my guest!"
    mainLoop (M.empty)