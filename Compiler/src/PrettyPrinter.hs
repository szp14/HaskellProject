{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PrettyPrinter where

import Text.PrettyPrint as T
-- import Text.PrettyPrint.GenericPretty

import Grammar

import qualified Data.Set as Set
import Control.Applicative
import Data.Attoparsec.Text 
import Data.Text.Internal as Inter
import Data.Text
import Data.Functor


debuger :: (Show t) => Either String t -> String
debuger (Right t) = show t
debuger (Left s) = s


-- BoolExpr
instance Show BoolExpr where
    show = render . flip printBool 0

printBool :: BoolExpr -> Int -> Doc

printBool (BVar v) n = nest n (T.text "var" <+> T.text (show v))
printBool (FalseLit) n = nest n (T.text "False")
printBool (TrueLit) n = nest n (T.text "True")
printBool (Not v) n = nest n (T.text "not")
            $+$ printBool v (n + 3)
printBool (And l r) n = nest n (T.text "and")
            $+$ printBool l (n + 3) $+$ printBool r (n + 3)
printBool (Or l r) n = nest n (T.text "or")
            $+$ printBool l (n + 3) $+$ printBool r (n + 3)

            
-- NumExpr
instance Show NumExpr where
    show = render . flip printNum 0
    
printNum :: NumExpr -> Int -> Doc

printNum (NVar v) n = nest n (T.text "var" <+> T.text (show v))
printNum (Number v) n = nest n (T.double v)
printNum (Add l r) n = nest n (T.text "+")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (Sub l r) n = nest n (T.text "-")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (Mul l r) n = nest n (T.text "*")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (Div l r) n = nest n (T.text "/")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (E l r)  n = nest n (T.text "=")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (L l r)  n = nest n (T.text "<")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (LE l r) n = nest n (T.text "<=")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (G l r)  n = nest n (T.text ">")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
printNum (GE l r) n = nest n (T.text ">=")
            $+$ printNum l (n + 3) $+$ printNum r (n + 3)
         
         
-- ListExpr
instance Show ListExpr where
    show = render . flip printList 0
    
printList :: ListExpr -> Int -> Doc

printList (LVar v) n = nest n (T.text "var" <+> T.text (show v))
printList (Nil) n = nest n (T.text "Nil")
printList (Cons l r) n = nest n (T.text "cons")
            $+$ printList l (n + 3) $+$ printList r (n + 3)
printList (Car v) n = nest n (T.text "car")
            $+$ printList v (n + 3)
printList (Cdr v) n = nest n (T.text "cdr")
            $+$ printList v (n + 3)
printList (CharLit v) n = nest n (T.text "char" <+> T.char v)
printList (StringLit v) n = nest n (T.text "string" <+> T.text v)
            
            
-- Expr 
instance Show Expr where
    show = render . flip printExpr 0

printExpr :: Expr -> Int -> Doc

printExpr (EVar v) n = nest n (T.text "var" <+> T.text (show v))
printExpr (BExp v) n = nest n (T.text "bool")
            $+$ printBool v (n + 3)
printExpr (NExp v) n = nest n (T.text "num")
            $+$ printNum v (n + 3)
printExpr (LExp v) n = nest n (T.text "list")
            $+$ printList v (n + 3)
            
            
-- Stmt
instance Show Stmt where
    show = render . flip printStmt 0

printStmt :: Stmt -> Int -> Doc

printStmt (Exp v) n = nest n (T.text "exp")
            $+$ printExpr v (n + 3)
printStmt (Stmt_cycle v) n = nest n (T.text "stmt_cycle")
            $+$ printSL v (n + 3)
printStmt (Assign l r) n = nest n (T.text "assign")
            $+$ nest (n + 3) (T.text (show l)) $+$ printExpr r (n + 3)
printStmt (Skip) n = nest n (T.text "skip")
printStmt (If v l r) n = nest n (T.text "if")
            $+$ printExpr v (n + 3) $+$ printStmt l (n + 3) $+$ printStmt r (n + 3)
printStmt (While l r) n = nest n (T.text "while")
            $+$ printExpr l (n + 3) $+$ printStmt r (n + 3)
printStmt (New_vec l r) n = nest n (T.text "new_vec")
            $+$ nest (n + 3) (T.text (show l)) $+$ printExpr r (n + 3)
printStmt (Set_vec v l r) n = nest n (T.text "set_vec")
            $+$ nest (n + 3) (T.text (show v)) $+$ printExpr l (n + 3) $+$ printExpr r (n + 3)


-- Stmt_list
instance Show Stmt_list where
    show = render . flip printSL 0

printSL :: Stmt_list -> Int -> Doc

printSL (One v) n = nest n (T.text "one")
        $+$ printStmt v (n + 3)
printSL (Cycle l r) n = nest n (T.text "cycle")
        $+$ printStmt l (n + 3) $+$ printSL r (n + 3)

prettyPrinter :: [String] -> Either String [String]
prettyPrinter [] = Right []
prettyPrinter (s : ss) = do
    let headList = debuger (Grammar.parse s)
    tailList <- prettyPrinter ss
    return (headList : tailList)