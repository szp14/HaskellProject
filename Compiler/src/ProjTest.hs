module ProjTest where

import Test.QuickCheck
import Data.Char
import Data.Eq
import Interpreter

import qualified Data.Map as M
import System.IO
import Grammar
import qualified Data.Text
import qualified Data.Vector as V

quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n, maxDiscardRatio = 10000 * n }

res :: Either String [String] -> String
res (Right s) = head s
res (Left s) = s

ans :: Either String ([String], Env) -> String
ans (Right s) = show (snd s)
ans (Left s) = s



p00 = res (interpreter ["True "] M.empty) == "True"
p01 = res (interpreter ["False"] M.empty) == "False"

p10 = res (interpreter ["(and True  True )"] M.empty) == "True"
p11 = res (interpreter ["(and False True )"] M.empty) == "False"
p12 = res (interpreter ["(and True  False)"] M.empty) == "False"
p13 = res (interpreter ["(and False False)"] M.empty) == "False"
p14 = res (interpreter ["(or  True  True )"] M.empty) == "True"
p15 = res (interpreter ["(or  False True )"] M.empty) == "True"
p16 = res (interpreter ["(or  True  False)"] M.empty) == "True"
p17 = res (interpreter ["(or  False False)"] M.empty) == "False"
p18 = res (interpreter ["(not True )"] M.empty) == "False"
p19 = res (interpreter ["(not False)"] M.empty) == "True"

-- p20

p30 x   = (x >= 0) ==> res (interpreter [show x] M.empty) == show x
            where types = (x::Double)
p31 x y = (x >= 0 && y >= 0) ==> res (interpreter
            ["(+ " ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x + y)
            where types = (x :: Double, y :: Double)
p32 x y = (x >= 0 && y <= x) ==> res (interpreter
            ["(- " ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x - y)
            where types = (x :: Double, y :: Double)
p33 x y = (x >= 0 && y >= 0) ==> res (interpreter
            ["(* " ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x * y)
            where types = (x :: Double, y :: Double)
p34 x y = (x >= 0 && y >  0) ==> res (interpreter
            ["(/ " ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x / y)
            where types = (x :: Double, y :: Double)

p35 x y = (x >= 0 && y >= 0) ==> res (interpreter
            ["(= "  ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x == y)
            where types = (x :: Double, y :: Double)
p36 x y = (x >= 0 && y >= 0) ==> res (interpreter
            ["(> "  ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x > y)
            where types = (x :: Double, y :: Double)
p37 x y = (x >= 0 && y >= 0) ==> res (interpreter
            ["(< "  ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x < y)
            where types = (x :: Double, y :: Double)
p38 x y = (x >= 0 && y >= 0) ==> res (interpreter
            ["(>= " ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x >= y)
            where types = (x :: Double, y :: Double)
p39 x y = (x >= 0 && y >= 0) ==> res (interpreter
            ["(<= " ++ show x ++ " " ++ show y ++ ")"] M.empty) == show (x <= y)
            where types = (x :: Double, y :: Double)
            
-- p40

p50 c = (isAlphaNum c && isAscii c) ==>
            (read (res (interpreter [show c] M.empty)) :: String) == [c]
            where types = (c :: Char)
p51   = res (interpreter ["nil"] M.empty) == "[]"
p52 s = (all isAlphaNum s && all isAscii s) ==>
            (read (res (interpreter [show s] M.empty)) :: String) == s
            where types = (s :: String)
p53 x y = (all isAlphaNum x && all isAscii x && all isAlphaNum y && all isAscii y) ==>
            (read (res (interpreter["(cons " ++ show x ++ " " ++ show y ++ ")"] M.empty)) :: String) == x ++ y
            where types = (x :: String, y :: String)
p54 x y = (all isAlphaNum x && all isAscii x && all isAlphaNum y && all isAscii y) ==>
            (read (res (interpreter ["(car (cons " ++ show x ++ " " ++ show y ++ "))"] M.empty)) :: String) == x
            where types = (x :: String, y :: String)
p55 x y = (all isAlphaNum x && all isAscii x && all isAlphaNum y && all isAscii y) ==>
            (read (res (interpreter ["(cdr (cons " ++ show x ++ " " ++ show y ++ "))"] M.empty)) :: String) == y
            where types = (x :: String, y :: String)
            
-- p60

p70 = ans (eval (Assign t v) e) == "fromList [(\"a\"," ++ show 2.0 ++ ")]"
            where t = (TypeNum (Data.Text.pack "a"))
                  v = (NExp (Number 2))
                  e = ([], M.fromList [("a", (NumVal 1))])
p71 = ans (eval (Skip) e) == "fromList [(\"a\"," ++ show 1.0 ++ ")]"
            where e = ([], M.fromList [("a", (NumVal 1))])
p72 = ans (eval (If v t f) e) == "fromList [(\"a\"," ++ show 2.0 ++ ")]"
            where v = (BExp TrueLit)
                  t = (Assign (TypeNum (Data.Text.pack "a")) (NExp (Number 2)))
                  f = (Assign (TypeNum (Data.Text.pack "a")) (NExp (Number 3)))
                  e = ([], M.fromList [("a", (NumVal 1))])
p73 = ans (eval (If v t f) e) == "fromList [(\"a\"," ++ show 3.0 ++ ")]"
            where v = (BExp FalseLit)
                  t = (Assign (TypeNum (Data.Text.pack "a")) (NExp (Number 2)))
                  f = (Assign (TypeNum (Data.Text.pack "a")) (NExp (Number 3)))
                  e = ([], M.fromList [("a", (NumVal 1))])
p74 = ans (eval (While v t) e) == "fromList [(\"a\"," ++ show False ++ ")]"
            where v = (BExp (BVar (TypeBool (Data.Text.pack "a"))))
                  t = (Assign (TypeBool (Data.Text.pack "a")) (BExp FalseLit))
                  e = ([], M.fromList [("a", (BoolVal True))])
            
            
-- p80 = ans (eval (New_vec (TypeNum (Data.Text.pack "a")) (NExp (Number 1.0))) ([], M.fromList []))


checkAll = do
            putStrLn "12 tests for bool (and/or/not) :"
            quickCheck p00
            quickCheck p01
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
            putStrLn "10 * 100 tests for num (+-*/ <>=) :"
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
            putStrLn "some tests for list (cons/car/cdr) :"
            quickCheck p50
            quickCheck p51
            quickCheckN 50 p52
            quickCheckN 3  p53
            quickCheckN 3  p54
            quickCheckN 3  p55
            putStrLn "5 tests for While (set!/skip/if/while) :"
            quickCheck p70
            quickCheck p71
            quickCheck p72
            quickCheck p73
            quickCheck p74