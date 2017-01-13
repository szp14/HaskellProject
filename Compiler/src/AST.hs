{-# LANGUAGE OverloadedStrings #-}

module AST where

import Control.Applicative
import Data.Attoparsec.Text 
import Data.Text.Internal as Inter
import Data.Functor
import Data.Text
import Lexical

parseAST :: String -> Either String Stmt
parseAST str = do
    ans <- parseOnly stmtParser (pack str)
    return ans