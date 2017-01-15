{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Map as M
import System.IO
import Grammar
import Data.Text
import qualified Data.Vector as V

data Value
    = BoolVal {boolVal :: Bool}
    | NumVal {numVal :: Double}
    | CharVal {charVal :: Char}
    | ListVal {listVal :: [Value]}
    | VectorVal {vecVal :: (V.Vector Value), vecLen :: Int}

instance Show Value where
    show (BoolVal val) = show val
    show (NumVal val) = show val
    show (CharVal val) = show val
    show (ListVal val) = show val
    show (VectorVal val idx) = show val

type Env = M.Map String Value

getBool :: BoolExpr -> Env -> Either String Value
getBool FalseLit env = Right (BoolVal False)
getBool TrueLit env = Right (BoolVal True)
getBool (Not p) env = do
    BoolVal val <- getExpr p env
    return (BoolVal (not val))
getBool (And p q) env = do 
    BoolVal val1 <- getExpr p env
    BoolVal val2 <- getExpr q env
    return (BoolVal (val1 && val2))
getBool (Or p q) env = do
    BoolVal val1 <- getExpr p env
    BoolVal val2 <- getExpr q env
    return (BoolVal (val1 || val2))
getBool (BVar v@(TypeBool var)) env = do
    case M.lookup (show v) env of 
        Just (BoolVal val) -> do
            return (BoolVal val)
        Nothing -> do
            Left ("The variable " ++ (show var) ++ " is undefined!")
        Just _ -> do
            Left ("The type of variable " ++ (show var) ++ " should be Bool!")

getNum :: NumExpr -> Env -> Either String Value
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

getList :: ListExpr -> Env -> Either String Value
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

getExpr :: Expr -> Env -> Either String Value
getExpr (EVar var) env = do
    case M.lookup (show var) env of
        Just val@(VectorVal vec len) -> case var of
            TypeVector _ expr -> do
                (NumVal idxVal) <- getExpr expr env
                let index = floor idxVal
                case compare index len of
                    LT -> case compare index 0 of
                        LT -> do
                            Left ("The index is out of range!")
                        _ -> do
                            return (vec V.! index)
                    _ -> do
                        Left ("The index is out of range!")
            TypeUnknown _ -> do
                return val
        Just val -> do
            return val
        Nothing -> do
            Left ("The variable " ++ (show var) ++ " is undefined!")
getExpr (BExp expr) env = getBool expr env
getExpr (NExp expr) env = getNum expr env
getExpr (LExp expr) env = getList expr env

eval :: Stmt -> ([String], Env) -> Either String ([String], Env)
eval stmt ans@(strList, env) = case stmt of
    Exp expr -> do
        val <- getExpr expr env
        return ((show val) : strList, env)
    Stmt_cycle stmtList -> case stmtList of
        One stmt -> eval stmt ans
        Cycle stmt stmtList2 -> do
            newAns <- eval stmt ans
            eval (Stmt_cycle stmtList2) newAns
    Assign var expr -> do
        val <- getExpr expr env
        --let str = ("variable " ++ (show var) ++ " is set to " ++ (show val))
        --return (str : strList, (M.insert (show var) val env))
        return (strList, (M.insert (show var) val env))
    Skip -> do
        return ans
    If expr stmtT stmtF -> do
        (BoolVal val) <- getExpr expr env
        case val of
            True -> do
                newAns <- eval stmtT ans
                return newAns
            False -> do
                newAns <- eval stmtF ans
                return newAns
    While expr stmt -> do
        (BoolVal val) <- getExpr expr env
        case val of 
            True -> do
                newAns <- eval stmt ans
                eval (While expr stmt) newAns
            False -> do
                return ans
    New_vec var expr -> do
        (NumVal val) <- getExpr expr env
        let len = floor val
        --let str = ("vector " ++ (show var) ++ " is created")
        --return (str : strList, (M.insert (show var) (VectorVal V.empty len) env))
        return (strList, (M.insert (show var) (VectorVal V.empty len) env))
    Set_vec var exprLen exprVal -> case M.lookup (show var) env of 
        Just (VectorVal vec len) -> do
            (NumVal idx) <- getExpr exprLen env
            let index = floor idx
            val <- getExpr exprVal env
            case compare index len of
                LT -> case compare index 0 of
                    LT -> do
                        Left ("The index is out of range!")
                    _ -> case V.null vec of
                        True -> do
                            return (strList, (M.insert (show var) (VectorVal (V.replicate len val) len) env))
                        False -> do
                            return (strList, (M.insert (show var) (VectorVal (vec V.// [(index, val)]) len) env))
                _ -> do
                    Left ("The index is out of range!")

interpreter :: [String] -> Env -> Either String [String]
interpreter [] env = Right []
interpreter (s : ss) env = do
    stmt <- Grammar.parse s
    (headList', newEnv) <- eval stmt ([], env)
    let headList = Prelude.reverse headList'
    tailList <- interpreter ss newEnv
    return (headList ++ tailList)