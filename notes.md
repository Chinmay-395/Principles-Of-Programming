**EX 1.3.1** What is the difference between a result and an expressed value?

The subset of results that are integers are called expressed values: it is the name given to non-
error results of evaluation.

**EX 1.3.2**
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
# Doubt

**Please explain again**

```ocaml
interp "let f = proc (g) {proc (x) {if zero?(x) then 1 else x*((g g) (x-1)}} in ((f f) 5)";;
```
its called the "higher order trick"

'g' is running 'g' the same as 'f' is running 'f'
------------------------------------------------

# Doubt

explain the 'rec' language

------------------------------------------------

