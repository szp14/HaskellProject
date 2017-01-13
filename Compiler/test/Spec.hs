module ProjectTest where

import Test.QuickCheck
import Parser

import Control.Applicative
import Data.Attoparsec.Text 
import Data.Text
import Data.Text.Internal as Inter
import Data.Functor

p00 = boolParser (pack "True")    == "True"
p01 = boolParser (pack "False")   == "False"

p10 = boolParser (pack "(and True  True)")   == "True"
p11 = boolParser (pack "(and False True)")   == "False"
p12 = boolParser (pack "(and True  False)")  == "False"
p13 = boolParser (pack "(and False False)")  == "False"
p14 = boolParser (pack "(or  True  True)")   == "True"
p15 = boolParser (pack "(or  False True)")   == "True"
p16 = boolParser (pack "(or  True  False)")  == "True"
p17 = boolParser (pack "(or  False False)")  == "False"
p18 = boolParser (pack "(not True)")         == "False"
p19 = boolParser (pack "(not False)")        == "True"

-- p20 = 

p30 x   = (x >= 0) ==> numParser (pack (show x)) == "Right " ++ show x
            where types = (x::Double)
p31 x y = (x >= 0 && y >= 0) ==> numParser (pack ("(+ " ++ show x ++ " " ++ show y ++ ")")) == "Right " ++ show (x + y)
            where types = (x :: Double, y :: Double)
p32 x y = (x >= 0 && y <= x) ==> numParser (pack ("(- " ++ show x ++ " " ++ show y ++ ")")) == "Right " ++ show (x - y)
            where types = (x :: Double, y :: Double)
p33 x y = (x >= 0 && y >= 0) ==> numParser (pack ("(* " ++ show x ++ " " ++ show y ++ ")")) == "Right " ++ show (x * y)
            where types = (x :: Double, y :: Double)
p34 x y = (x >= 0 && y > 0) ==> numParser (pack ("(/ " ++ show x ++ " " ++ show y ++ ")"))  == "Right " ++ show (x / y)
            where types = (x :: Double, y :: Double)

p35 x y = (x >= 0 && y >= 0) ==> numParser (pack ("(= " ++ show x ++ " " ++ show y ++ ")"))  == "Left " ++ show (x == y)
            where types = (x :: Double, y :: Double)
p36 x y = (x >= 0 && y >= 0) ==> numParser (pack ("(> " ++ show x ++ " " ++ show y ++ ")"))  == "Left " ++ show (x > y)
            where types = (x :: Double, y :: Double)
p37 x y = (x >= 0 && y >= 0) ==> numParser (pack ("(< " ++ show x ++ " " ++ show y ++ ")"))  == "Left " ++ show (x < y)
            where types = (x :: Double, y :: Double)
p38 x y = (x >= 0 && y >= 0) ==> numParser (pack ("(>= " ++ show x ++ " " ++ show y ++ ")")) == "Left " ++ show (x >= y)
            where types = (x :: Double, y :: Double)
p39 x y = (x >= 0 && y >= 0) ==> numParser (pack ("(<= " ++ show x ++ " " ++ show y ++ ")")) == "Left " ++ show (x <= y)
            where types = (x :: Double, y :: Double)

checkAll = do
            quickCheck p00
            quickCheck p01
            putStrLn "10 tests for bool (and/or/not) :"
            quickCheck p10
            quickCheck p11
            quickCheck p12
            quickCheck p13
            quickCheck p14
            quickCheck p15
            quickCheck p16
            quickCheck p17
            quickCheck p18
            quickCheck p19
            putStrLn "10 tests for num (+-*/ <>=) :"
            quickCheck p30
            quickCheck p31
            quickCheck p32
            quickCheck p33
            quickCheck p34
            quickCheck p35
            quickCheck p36
            quickCheck p37
            quickCheck p38
            quickCheck p39