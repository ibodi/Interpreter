{-# LANGUAGE Safe #-}
module BohdanShcherbakTests(tests) where

import DataTypes

tests :: [Test]
tests =
  [  Test "inc"      (SrcString "input x in x + 1024") (Eval [0] (Value 1024))
  	,Test "undefVar" (SrcString "x") TypeError
  	,Test "minus one" (SrcString "input x in x - 1") (Eval [0] (Value (-1)))
	,Test "second one" (SrcString "input x itsover in if -(3 > x) then 7 else itsover") TypeError
	,Test "no arguments" (SrcString "if false then 1 div 0 else 777") (Eval [] (Value 777))
	,Test "anything" (SrcString "input x in x * x + x div x - x") (Eval [2] (Value 3))
	,Test "just numbers" (SrcString "1 + 2 - 3 * 4 div 5") (Eval [] (Value 1))
	,Test "with undefined variable" (SrcString "input a b in a + b - c") TypeError
	,Test "not used variables" (SrcString "input a b c d e f g in if if false then true else true then 123 else 321") (Eval [] (Value 123))
	,Test "let" (SrcString "input c in let v = 3*c in 2+v") (Eval [9] (Value 29))
	,Test "large sum" (SrcString "input q w e r t y u i o p a s d f g h j k l c n z in q + w + d + h + k + c + n + z") (Eval [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] (Value 8))
	,Test "zero" (SrcString "input x in 0 * x * x * x + x - x") (Eval [347] (Value 0))
	,Test "undefined variable" (SrcString "variable") TypeError
	,Test "a number" (SrcString "3") (Eval [] (Value 3))
  	,Test "Runtime Error" (SrcString "input x y in let n = x*y in (if n > 33 then x-y else x+y) div (let i = 0 in i)") (Eval [9,23] RuntimeError)
	,Test "let bool" (SrcString "input x y in let x = (y > 2) in if x then y*5 else 0") (Eval [5,3] (Value 15))
	,Test "minus without ()'s" (SrcString "123 + -42 + -76") (Eval [] (Value 5))
	,Test "a lot of minuses" (SrcString "- - - - - - - - - - 1234") (Eval [] (Value 1234))
	,Test "number as condition" (SrcString "if 777 then 0 else 0") TypeError
	,Test "nots" (SrcString "if not not not not true then 1234 else 4321") (Eval [] (Value 1234))
	,Test "1 + true" (SrcString "1 + true") TypeError
	,Test "dividing by zero" (SrcString "6 div 0") (Eval [] RuntimeError)
  	,Test "Poland is the best" (SrcString "input Poland is the best in 42") (Eval [42,42,42,42] (Value 42))
	,Test "let in let" (SrcString "let x = 10 in let y = 20 in if x > y then x else y") (Eval [] (Value 20))
	,Test "bool return" (SrcString "2 = 3 and true or false") TypeError
	,Test "if with different types" (SrcString "if 2<>3 then 42 else true and 4=5") TypeError
	,Test "nested ifs" (SrcString "if if if if if if if if 7=7 then true else false then true else false then true else false then true else false then true else false then true else false then true else false then 1 else 0") (Eval [] (Value 1))
	,Test "the last one" (SrcString "input love be wIth you in let love = 63 in love + be + wIth + you") (Eval [5,5,5,5] (Value 78))
	
	,Test "boolToInt" (SrcString "fun boolToInt(x : bool):int = if x then 1 else 0 in boolToInt(true)") (Eval [] (Value 1)) -- ????
	,Test "to unit and back" (SrcString "fun intToUnit(x : int) : unit = () fun uti(u: unit): int = 0 in uti(intToUnit(5))") (Eval [] (Value 0))
	,Test "identity  " (SrcString "fun idInt(x : int) : int = x input x in idInt(x)") (Eval [8] (Value 8))
	,Test "identity err " (SrcString "fun idInt(x : int) : int = x input x in idInt(x div 0)") (Eval [8] RuntimeError)
	,Test "odd check" (SrcString "fun odd(x : int) : bool = not (even(x)) fun even(x : int) : bool = x mod 2 = 0 fun boolToInt(x : bool):int = if x then 1 else 0 input x in boolToInt(odd(x))") (Eval [5] (Value 1))
	,Test "foo bar" (SrcString "fun foo(x : int) : int = x + 3 fun bar(x : int) : int = x * 2 input x in foo(bar(x))") (Eval [5] (Value 13))
	,Test "foo bar   2" (SrcString "fun foo(x : int) : int = x + 3 fun bar(x : int) : int = x * 2 input x in foo(bar(x div 0))") (Eval [5] RuntimeError)
	,Test "boolToInt" (SrcString "fun boolToInt(x : bool) : int = if x then 1 else 0 in boolToInt(y)") TypeError
	,Test "to unit and back" (SrcString "fun intToUnit(x : int) : unit = () fun uti(u: unit): int = 0 in uti(intToUnit(x))") TypeError
	,Test "identity  " (SrcString "fun idInt(x : int) : int = x in idInt(x)") TypeError
	,Test "2^3" (SrcFile "pow.pp5") (Eval [2,3] (Value 8))
	,Test "1234^0" (SrcFile "pow.pp5") (Eval [1234,0] (Value 1))
	,Test "10^4" (SrcFile "pow.pp5") (Eval [10,4] (Value 10000))
	,Test "10^(-6)" (SrcFile "pow.pp5") (Eval [10,-6] RuntimeError)
	,Test "fifth el of list" (SrcFile "nth.pp5") (Eval [5] (Value 8))
	,Test "listOdd 1" (SrcFile "listOdd.pp5") (Eval [1,3,5,7,9] (Value 1))
	,Test "listOdd 2" (SrcFile "listOdd.pp5") (Eval [1,2,3,4,5] (Value 0))
	,Test "last take Ok" (SrcFile "lastTake.pp5") (Eval [4] (Value 4))
	,Test "last take ErrShortList" (SrcFile "lastTake.pp5") (Eval [25] RuntimeError)
	,Test "append test" (SrcFile "append.pp5") (Eval [] (Value 6))
	,Test "arithm    " (SrcFile "arithm.pp5") (Eval [1,2,3,4,5] (Value (-1)))
	,Test "fibonacci'ego" (SrcFile "fib2.pp5") (Eval [4] (Value 3))
	,Test "reverse" (SrcFile "reverse.pp5") (Eval [] (Value 8))
	,Test "pairToList" (SrcFile "pairToList.pp5") (Eval [] (Value 1))
	,Test "sum list" (SrcFile "sum.pp5") (Eval [] (Value 15))
  ]