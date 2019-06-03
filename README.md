
## Interpreter of a simple functional programming language

The only files in this project that was written by me are `BohdanShcherbak.hs`, `BohdanShcherbakTests.hs` and all the files with `.pp5` extension. All the other files are written by employees of Institute of Computer Science of University of Wroclaw. The file `BohdanShcherbak.hs` is the solution to the task defined in  `Tasks/prac5.pdf` (in Polish). It contains an interpreter of a simple programming language which is described in files `Tasks/prac4.pdf` and `Tasks/prac5.pdf`. Files with `.pp5` extension are examples of programs written in this language. The file `BohdanShcherbakTests.hs` contains tests for the solution which use programs in the `.pp5` files.

A program written in this language consists of list of functions and a line:
```
input [list of variables] in [expression that may contain those variables and functions]
```
Example program:
```
fun boolToInt(x : bool) : int = if x then 1 else 0
fun listOdd(xs: int list): bool =
	match xs with 
		| [] -> true
		| y::ys -> odd(y) and listOdd(ys)
fun odd(x : int) : bool = not (even(x))
fun even(x : int) : bool = x mod 2 = 0

input a b c d e in boolToInt(listOdd([a,b,c,d,e] : int list))
```

In order to run tests you have to install `ghc` and run the following commands in the terminal in the root of the repo:

```
$ make
$ ./Prac5 -t
```

In order to run an example program `listOdd.pp5` run the following commands in the terminal in the root of the repo:

```
$ make
$ ./Prac5 listOdd.pp5
```

Then you would have to enter values of every input variable in the list of variables of the input instruction of the program. Then you will see the result of evaluation of the expression of the input instruction. For example:
```
a = 3
b = 5
c = 2
d = 5
e = 6
Value 0
```
