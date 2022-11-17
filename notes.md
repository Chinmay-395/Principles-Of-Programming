# Implementation running of different example

## **implemented expressed values and environments**

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

---

## **Implemented isZero and ifThenElse**

```bash
─( 00:38:16 )─< command 14 >─────────────────────────────────────{ counter: 0 }─
utop # interp "if zero?(5-5) then 2 else 4";;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 2)
```

---

setup debugger in ocaml

WHAT I LEARNED:

1. _homework 3 should be built on **LET**_
2. deploy this language
3. USE reason ML for debugging with itellij

- A closure is a triple with argument

- `type` in OCAML is mutually recurssive therefore we use "and" (code ref: ast.ml in Proc `type exp_val`)

---

## **implemented Let**

```bash
─( 14:19:47 )─< command 0 >──────────────────────────────────────{ counter: 0 }─
utop # interp "
let x =2
in let y =3
in x + y " ;;
- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 5)
```

## **The rec example**

─( 12:03:58 )─< command 13 >─────────────────────────────────────{ counter: 0 }─
utop # interp "
letrec fact ( x ) =
if zero?( x )
then 1
else x \* (fact (x-1))
in (fact 3)";;

- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 6)

---

## **the dynamic scoping problem**

─( 13:21:25 )─< command 1 >──────────────────────────────────────{ counter: 0 }─
utop # interp "
let f = proc (x) { if zero?(x) then 1 else x\*(f(x -1)) }
in (f 6)";;

- : exp_val Dysfunctionally_Functional.Ds.result = Ok (NumVal 720)
  ─( 13:21:39 )─< command 2 >──────────────────────────────────────{ counter: 0 }─
  utop # interp "
  let f = let a =2 in proc ( x ) { x + a }
  in (f 2) ";;
- : exp_val Dysfunctionally_Functional.Ds.result = Error "a not found!"

---

###### EX 1.3.1

---

What i`s the difference between a result and an expressed value?
The subset of results that are integers are called expressed values: it is the name given to non-
error results of evaluation.

---

###### EX 1.3.2

---

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

---

# Doubt --> (Done)

---

## the proc language

**Please explain again**

```ocaml
interp "let f = proc (g) {proc (x) {if zero?(x) then 1 else x*((g g) (x-1)}} in ((f f) 5)";;
```

its called the "higher order trick"

## 'g' is running 'g' the same as 'f' is running 'f'

## explain the 'rec' language

## implementation of untuple

## the mutually recurrsive `and`

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

interp " addq (4 , addq (3 , addq (2 , addq (1 , emptyqueue ))))";;

```bash
─( 10:36:26 )─< command 3 >────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let x =  ref 0;;
val x : int ref = {contents = 0}
─( 10:36:26 )─< command 4 >────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # x;;
- : int ref = {contents = 0}
─( 10:43:11 )─< command 5 >────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # ref;;
- : 'a -> 'a ref = <fun>
─( 10:43:12 )─< command 6 >────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let x =  ref 'hello';;
Error: Syntax error
─( 10:43:45 )─< command 7 >────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let x =  ref "hello";;
val x : string ref = {contents = "hello"}
─( 10:44:56 )─< command 8 >────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # (!);;
- : 'a ref -> 'a = <fun>
─( 10:45:11 )─< command 9 >────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # !x;;
- : string = "hello"
─( 10:47:32 )─< command 10 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # print_string;;
- : string -> unit = <fun>
─( 10:48:03 )─< command 11 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # (:=);;
- : 'a ref -> 'a -> unit = <fun>
─( 10:48:39 )─< command 12 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let x = ref 0;;
val x : int ref = {contents = 0}
─( 10:49:24 )─< command 13 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let y = ref "hello";;
val y : string ref = {contents = "hello"}
─( 10:50:02 )─< command 14 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # x:=2;;
- : unit = ()
─( 10:50:11 )─< command 15 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # x:=[1;2];;
Error: This expression has type 'a list but an expression was expected of type
         int
─( 10:50:27 )─< command 16 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # let f = ref (fun x -> x+1);;
val f : (int -> int) ref = {contents = <fun>}
─( 10:51:13 )─< command 17 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # (!f)3;;
- : int = 4
─( 10:52:04 )─< command 18 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # f:=(fun x -> x+2);;
- : unit = ()
─( 10:52:52 )─< command 19 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # f:=3;;
Error: This expression has type int but an expression was expected of type
         int -> int
─( 10:53:24 )─< command 20 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # x:=x+1;;
Error: This expression has type int ref but an expression was expected of type
         int
─( 10:53:34 )─< command 21 >───────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # x;;
- : int ref = {contents = 2}

```

Doubts -> Implicit Ref, Explicit Ref, Q2 & Q3 of Quiz on Nov 3 and Type Checking

### Show that it is typable

                                ---------:TVar                   ---------:TInt
                      {x:int,y:bool} |- x:int            {x:int,y:bool} |- x:int

---------:TVar --------- TVar --------- TSub  
{x:int,y:bool} |- y:bool {x:int,y:bool} |- x: int {x:int,y:bool} |- x-1: int
------------------------------- TITE
{x:int,y:bool} |- if y then x else x-1 : int
---------------------------------------------------------------- TProc
{x:int} |- proc (y:bool) { if y then x else x-1 } : bool -> int
---------------------------------------------------------------- TProc
{} |- proc (x:int) { proc (y:bool) { if y then x else x-1 } } : int -> bool -> int

using TApp we cannot do higher order trick
explain TRec
explain TProj
complete 4.1 bcoz it might be in asked next time -> 4.1.2 & 4.1.3

store is a heap
refVal is heap location thats why its an int

```
let l1 = { head <= 0 ; size <= 0}
(* 0 in head signals null *)
in let add_front = proc ( x ) { proc ( l ) {
begin
l.head<={data<=x;next<=l.head};
l.size<=l.size+1
end
} }
in begin
(( add_front 2) l1 ) ;
(( add_front 3) l1 ) ;
debug ( l1 ) (* required to inspect the list *)
end
```

```
let l1 = { head <= 0 ; size <= 0}
in let add_front = proc ( x ) { proc ( l ) {
begin
l . head <={ data <= x ; next <= l . head }
l . size <= l . size +1
end } }
in letrec bump_helper ( node ) =
if number ?( node )
then 0
else ( begin
node . data <= node . data + 1
( bump_helper node . next )
end )
in let bump = proc ( ll ) { ( bump_helper ll . head ) }
in begin
(( add_front 2) l1 )
(( add_front 3) l1 )
( bump l1 ) ;
debug ( l1 )
end
```
