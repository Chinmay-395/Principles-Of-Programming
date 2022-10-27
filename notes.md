# Implementation running of different example
**implemented expressed values and environments**
----------------------------------------
```bash
─( 23:53:34 )─< command 13 >─────────────────────────────────────{ counter: 0 }─
utop # interp "2+3";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 5)
─( 23:53:48 )─< command 14 >─────────────────────────────────────{ counter: 0 }─
utop # interp "2.4+.3.0";;
Exception: Failure "lexing: empty token".
─( 00:00:11 )─< command 15 >─────────────────────────────────────{ counter: 0 }─
utop # interp "5/2";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 2)
─( 00:00:19 )─< command 16 >─────────────────────────────────────{ counter: 0 }─
utop # interp "5*2";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 10)
─( 00:00:25 )─< command 17 >─────────────────────────────────────{ counter: 0 }─
utop # interp "abs(5-4)";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 1)
─( 00:00:30 )─< command 18 >─────────────────────────────────────{ counter: 0 }─
utop #
``` 
----------------------------------------

**Implemented isZero and ifThenElse**
----------------------------------------
```bash
─( 00:38:16 )─< command 14 >─────────────────────────────────────{ counter: 0 }─
utop # interp "if zero?(5-5) then 2 else 4";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 2)
```
----------------------------------------
setup debugger in ocaml

WHAT I LEARNED:

1) _homework 3 should be built on **LET**_
2) deploy this language  
3) USE reason ML for debugging with itellij

* A closure is a triple with argument

* `type` in OCAML is mutually recurssive therefore we use "and" (code ref: ast.ml in Proc `type exp_val`)

------------------------------------------------


**implemented Let**
------------------------------------------------
```bash
─( 14:19:47 )─< command 0 >──────────────────────────────────────{ counter: 0 }─
utop # interp "
let x =2
in let y =3
in x + y " ;;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 5)
```
**The rec example**
------------------------------------------------
─( 12:03:58 )─< command 13 >─────────────────────────────────────{ counter: 0 }─
utop # interp "
letrec fact ( x ) =
if zero?( x )
then 1
else x * (fact (x-1))
in (fact 3)";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 6)
------------------------------------------------
**the dynamic scoping problem**
------------------------------------------------
─( 13:21:25 )─< command 1 >──────────────────────────────────────{ counter: 0 }─
utop # interp "
let f = proc (x) { if zero?(x) then 1 else x*(f(x -1)) }
in (f 6)";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 720)
─( 13:21:39 )─< command 2 >──────────────────────────────────────{ counter: 0 }─
utop # interp "
let f = let a =2 in proc ( x ) { x + a }
in (f 2) ";;
- : exp_val Dysfunctionally_Functional.Ds.result = Error "a not found!"
------------------------------------------------

###### EX 1.3.1 
----------------------------------------
What i`s the difference between a result and an expressed value?
The subset of results that are integers are called expressed values: it is the name given to non-
error results of evaluation.
----------------------------------------

###### EX 1.3.2
--------------------------------------
```bash
─( 16:09:46 )─< command 0 >──────────────────────────────────────{ counter: 0 }─
utop # interp "1+2";;
- : int Dysfunctionally_Functional.Ds.result = Ok 3
─( 16:09:46 )─< command 1 >──────────────────────────────────────{ counter: 0 }─
utop # interp "8*2";;
- : int Dysfunctionally_Functional.Ds.result = Ok 16
─( 16:14:40 )─< command 13 >─────────────────────────────────────{ counter: 0 }─
utop # interp "abs(4-5)";;
- : int Dysfunctionally_Functional.Ds.result = Ok 1
```
----------------------------------------

# Doubt --> (Done)

-----------------------------------------------
the proc language
-----------------------------------------------
**Please explain again**

```ocaml
interp "let f = proc (g) {proc (x) {if zero?(x) then 1 else x*((g g) (x-1)}} in ((f f) 5)";;
```
its called the "higher order trick"

'g' is running 'g' the same as 'f' is running 'f'
------------------------------------------------
explain the 'rec' language
------------------------------------------------
implementation of untuple
------------------------------------------------
the mutually recurrsive `and`
------------------------------------------------


# Need to do

1. unpair --> (Done)
2. dynamic scooping --> (Haven't done)
3. 2.4.8 even & odd --> (Haven't done)
4. 2.4.9 --> Done
5. 2.4.10 --> Done with HW3
6. 2.5.2 --> Done

```
interp "
let one =1
in letrec fact ( x ) =
if zero ?( x )
then one
else x * ( fact (x -1))
in debug (( fact 6)) " ; ;
```



# Doubt --> in-progress (sent to Ramana)

1. The program does run inside my current language implmentation and produces the error mentioned below
```bash
─( 13:07:35 )─< command 1 >──────────────────────────────────────────────{ counter: 0 }─
utop # interp "cons(1,emptylist)";;
Exception: Dysfunctionally_Functional.Parser.MenhirBasics.Error.
```

###### Exercise 2.5.3

interp "
let l = cons (1 , cons (2 , cons (1 , emptylist )))
in let is_one = proc ( x ) { zero?(x-1)}
in letrec filter(l) = proc(f){
    if empty?(l) 
      then emptylist
    else (
        if (f hd(l)) 
          then 
            cons(hd(l), 
            (filter tl(l)) f))
        else (filter tl(l) f)
}";;

letrec foldr(l) = proc(f){
    proc(a){
        if empty?(l) 
          then a 
        else 
          ((f hd(l)) (((foldr tl(l))f)a) )
    }
    
}";;