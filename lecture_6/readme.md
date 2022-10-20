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

