{-# LANGUAGE OverloadedStrings #-}

module Lexical where

import qualified Data.Set as Set
import Control.Applicative
import Data.Attoparsec.Text 
import Data.Text.Internal as Inter
import Data.Text
import Data.Functor

keywords = Set.fromList ["True", "False", "nil", "cons", "car", "cdr", "set!"]

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

data Var
    = TypeBool Text
    | TypeNum Text
    | TypeList Text
    | TypeUnknown Text
    deriving Ord

instance Show Var where
    show (TypeBool str) = unpack str
    show (TypeNum str) = unpack str
    show (TypeList str) = unpack str
    show (TypeUnknown str) = unpack str

instance Eq Var where
    a == b = (show a) == (show b)

data BoolExpr = BVar Var
    | FalseLit
    | TrueLit
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    deriving Show

boolExprParser :: Parser BoolExpr
boolExprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser <|> boolVarParser

boolVarParser :: Parser BoolExpr
boolVarParser = do
    v <- lexeme letter
    vs <- Data.Attoparsec.Text.takeWhile $ inClass "0-9a-zA-Z"
    return (BVar (TypeBool (Data.Text.cons v vs)))

falseParser :: Parser BoolExpr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser BoolExpr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser BoolExpr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- boolExprParser
    lexeme $ char ')'
    return (Not expr)

andParser :: Parser BoolExpr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr0 <- boolExprParser
    expr1 <- boolExprParser
    lexeme $ char ')'
    return (And expr0 expr1)

orParser :: Parser BoolExpr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr0 <- boolExprParser
    expr1 <- boolExprParser
    lexeme $ char ')'
    return (Or expr0 expr1)

data NumExpr = NVar Var
    | Number Double
    | Add NumExpr NumExpr
    | Sub NumExpr NumExpr
    | Mul NumExpr NumExpr
    | Div NumExpr NumExpr
    | E NumExpr NumExpr
    | L NumExpr NumExpr
    | LE NumExpr NumExpr
    | G NumExpr NumExpr
    | GE NumExpr NumExpr
    deriving Show

numExprParser :: Parser NumExpr
numExprParser = floatParser <|> addParser <|> subParser <|> mulParser <|> divParser <|> equalParser <|> greatParser <|> lessParser <|> geParser <|> leParser <|> numVarParser

numVarParser :: Parser NumExpr
numVarParser = do
    v <- lexeme letter
    vs <- Data.Attoparsec.Text.takeWhile $ inClass "0-9a-zA-Z"
    return (NVar (TypeNum (Data.Text.cons v vs)))

floatParser :: Parser NumExpr
floatParser = do 
    skipSpace
    num <- double
    return (Number num)

addParser :: Parser NumExpr
addParser = do
    lexeme $ char '('
    lexeme $ char '+'
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (Add num1 num2)

subParser :: Parser NumExpr
subParser = do
    lexeme $ char '('
    lexeme $ char '-'
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (Sub num1 num2)

mulParser :: Parser NumExpr
mulParser = do
    lexeme $ char '('
    lexeme $ char '*'
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (Mul num1 num2)

divParser :: Parser NumExpr
divParser = do
    lexeme $ char '('
    lexeme $ char '/'
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (Div num1 num2)

equalParser :: Parser NumExpr
equalParser = do
    lexeme $ char '('
    lexeme $ char '='
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (E num1 num2)

greatParser :: Parser NumExpr
greatParser = do
    lexeme $ char '('
    lexeme $ char '>'
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (G num1 num2)

lessParser :: Parser NumExpr
lessParser = do
    lexeme $ char '('
    lexeme $ char '<'
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (L num1 num2)

geParser :: Parser NumExpr
geParser = do
    lexeme $ char '('
    lexeme $ string ">="
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (GE num1 num2)

leParser :: Parser NumExpr
leParser = do
    lexeme $ char '('
    lexeme $ string "<="
    num1 <- numExprParser
    num2 <- numExprParser
    lexeme $ char ')'
    return (LE num1 num2)

data ListExpr = LVar Var
    | Nil
    | Cons ListExpr ListExpr
    | Car ListExpr
    | Cdr ListExpr
    | CharLit Char
    | StringLit String
    deriving Show
  
listExprParser :: Parser ListExpr
listExprParser = charParser <|> stringParser <|> conParser <|> carParser <|> cdrParser <|> nullParser <|> listVarParser

listVarParser :: Parser ListExpr
listVarParser = do
    v <- lexeme letter
    vs <- Data.Attoparsec.Text.takeWhile $ inClass "0-9a-zA-Z"
    return (LVar (TypeList (Data.Text.cons v vs)))

charParser :: Parser ListExpr
charParser = do
    lexeme $ char '\''
    char1 <- anyChar
    lexeme $ char '\''
    return (CharLit char1)

nullParser :: Parser ListExpr
nullParser = do
    lexeme $ string "nil"
    return Nil

stringParser :: Parser ListExpr
stringParser = do
    lexeme $ char '\"'
    str <- takeTill (=='\"')
    lexeme $ char '\"'
    return (StringLit $ unpack str)

conParser :: Parser ListExpr
conParser = do
    lexeme $ char '('
    lexeme $ string "cons"
    list1 <- listExprParser
    list2 <- listExprParser
    lexeme $ char ')'
    return (Cons list1 list2)

carParser :: Parser ListExpr
carParser = do
    lexeme $ char '('
    lexeme $ string "car"
    list <- listExprParser
    lexeme $ char ')'
    return (Car list)

cdrParser :: Parser ListExpr
cdrParser = do
    lexeme $ char '('
    lexeme $ string "cdr"
    list <- listExprParser
    lexeme $ char ')'
    return (Cdr list)

data Expr
    = EVar Var
    | BExp BoolExpr
    | NExp NumExpr
    | LExp ListExpr
    deriving Show

exprParser :: Parser Expr
exprParser = varParser <|> bexpParser <|> nexpParser <|> lexpParser

varParser :: Parser Expr
varParser = do
    v <- lexeme letter
    vs <- Data.Attoparsec.Text.takeWhile $ inClass "0-9a-zA-Z"
    let var = Data.Text.cons v vs
    case Set.member (unpack var) keywords of
        True -> do
            fail ""
        False -> do
            return (EVar (TypeUnknown var))

bexpParser :: Parser Expr
bexpParser = do
    expr <- boolExprParser
    return (BExp expr)

nexpParser :: Parser Expr
nexpParser = do
    expr <- numExprParser
    return (NExp expr)

lexpParser :: Parser Expr
lexpParser = do
    expr <- listExprParser
    return (LExp expr)

data Stmt
    = Assign Var Expr
    | Exp Expr
    deriving Show

stmtParser :: Parser Stmt
stmtParser = stmtExprParser <|> assignParser

stmtExprParser :: Parser Stmt
stmtExprParser = do
    expr <- exprParser
    return (Exp expr)

assignParser :: Parser Stmt
assignParser = do
    lexeme $ char '('
    lexeme $ string "set!"
    (EVar var) <- varParser
    expr <- exprParser
    lexeme $ char ')'
    return (Assign var expr)