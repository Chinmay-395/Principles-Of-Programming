(* Name: Chinmay Dali *)
open Ast
open Ds


let rec apply_proc : exp_val -> exp_val -> exp_val ea_result =
  fun f a ->
  match f with
  | ProcVal (id,body,env) ->
    return env >>+
    extend_env id a >>+
    eval_expr body
  | _ -> error "apply_proc: Not a procVal"
and
 eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Abs(e1)      ->
    eval_expr e1  >>=
    int_of_numVal >>= fun n ->
    return @@ NumVal (abs n)
  | Cons(e1, e2) -> 
    eval_expr e1  >>= fun n ->
    eval_expr e2  >>= list_of_ListVal >>= fun m ->
      return (ListVal(n::m)) 
    
  | Hd(e1) ->  eval_expr e1 >>= list_of_ListVal >>= fun v3 -> return ((List.hd v3))
  | Tl(e1) ->  eval_expr e1 >>= list_of_ListVal >>= fun v3 -> return (ListVal(List.tl v3))
  | Empty(e1)  ->  eval_expr e1 >>=
        fun v1 -> if (isTreeVal v1) 
                      then  tree_of_TreeVal v1 >>= fun v3 -> return (BoolVal (v3 == Empty) )
                  else if(isList v1) 
                    then list_of_ListVal v1 >>= fun v3 -> return (BoolVal (v3 == [] ))
                  else error "Niether Tree nor List"
  | EmptyList    ->  return (ListVal [] )
  | EmptyTree ->  return (TreeVal(Empty))
  | Node(e1,lte,rte) ->  
    eval_expr e1 >>= fun x ->
    eval_expr lte >>= 
      tree_of_TreeVal >>= fun n2 ->
    eval_expr rte >>= 
      tree_of_TreeVal >>= fun n3 ->
        return (TreeVal(Node(x,n2,n3)))

  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->  
    eval_expr target >>= tree_of_TreeVal >>= fun t ->
      (match t with
      | Empty -> eval_expr emptycase
      | Node(x,n1,n2) ->
          extend_env id1 x >>+
          extend_env id2 (TreeVal n1) >>+
          extend_env id3 (TreeVal n2) >>+
          eval_expr nodecase)
  | _ -> failwith "Not implemented yet!"


(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)





let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf


(* Interpret an expression *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_expr
  in run c

(* Output  

─( 20:04:26 )─< command 2 >──────────────────────────────────────{ counter: 0 }─
utop # interp " abs (( -5)) - 6 ";;
- : exp_val Proc.Ds.result = Ok (NumVal (-1))
─( 20:09:38 )─< command 3 >──────────────────────────────────────{ counter: 0 }─
utop # interp " abs (7) - 6 ";;
- : exp_val Proc.Ds.result = Ok (NumVal 1)
─( 20:22:49 )─< command 4 >──────────────────────────────────────{ counter: 0 }─
utop # interp "cons(1,emptylist)";;
- : exp_val Proc.Ds.result = Ok (ListVal [NumVal 1])
─( 20:22:59 )─< command 5 >──────────────────────────────────────{ counter: 0 }─
utop # interp " cons ( cons (1 , emptylist ) , emptylist ) ";;
- : exp_val Proc.Ds.result = Ok (ListVal [ListVal [NumVal 1]])
─( 20:23:23 )─< command 6 >──────────────────────────────────────{ counter: 0 }─
utop # interp " let x = 4
in cons (x ,
cons ( cons (x -1 ,emptylist ) ,
emptylist )) " ;;
- : exp_val Proc.Ds.result = Ok (ListVal [NumVal 4; ListVal [NumVal 3]])
─( 20:23:35 )─< command 7 >──────────────────────────────────────{ counter: 0 }─
utop # interp " empty ?( emptylist ) " ;;
Exception: Proc.Lexer.Error "At offset 7: unexpected character.".
─( 20:23:50 )─< command 8 >──────────────────────────────────────{ counter: 0 }─
utop # interp " empty?(emptylist)";;
- : exp_val Proc.Ds.result = Ok (BoolVal true)
─( 20:23:59 )─< command 9 >──────────────────────────────────────{ counter: 0 }─
utop # interp " empty ?(tl(cons(cons(1,emptylist),emptylist)))";;
Exception: Proc.Lexer.Error "At offset 7: unexpected character.".
─( 20:24:15 )─< command 10 >─────────────────────────────────────{ counter: 0 }─
utop # interp " empty?(tl(cons(cons(1,emptylist),emptylist)))";;
- : exp_val Proc.Ds.result = Ok (BoolVal true)
─( 20:24:48 )─< command 11 >─────────────────────────────────────{ counter: 0 }─
utop # interp " tl ( cons ( cons (1 , emptylist ) , emptylist )) " ;;
- : exp_val Proc.Ds.result = Ok (ListVal [])
─( 20:24:54 )─< command 12 >─────────────────────────────────────{ counter: 0 }─
utop # interp " cons ( cons (1 , emptylist ) , emptylist ) " ;;
- : exp_val Proc.Ds.result = Ok (ListVal [ListVal [NumVal 1]])
─( 20:25:07 )─< command 13 >─────────────────────────────────────{ counter: 0 }─
utop # interp " emptytree " ;;
- : exp_val Proc.Ds.result = Ok (TreeVal Proc.Ds.Empty)
─( 20:25:16 )─< command 14 >─────────────────────────────────────{ counter: 0 }─
utop # interp " node (5 , node (6 , emptytree , emptytree ) , emptytree ) " ;;
- : exp_val Proc.Ds.result =
Ok
 (TreeVal
   (Proc.Ds.Node (NumVal 5,
     Proc.Ds.Node (NumVal 6, Proc.Ds.Empty, Proc.Ds.Empty), Proc.Ds.Empty)))
─( 20:25:26 )─< command 15 >─────────────────────────────────────{ counter: 0 }─
utop # interp "
caseT emptytree of {
emptytree -> emptytree ,
node (a ,l , r ) -> l
} " ;;
- : exp_val Proc.Ds.result = Ok (TreeVal Proc.Ds.Empty)
─( 20:25:34 )─< command 16 >─────────────────────────────────────{ counter: 0 }─
utop # interp "
let t = node(emptylist ,
            node(cons(5, cons(2, cons(1, emptylist))), 
                emptytree ,
                node(emptylist , 
                    emptytree , 
                    emptytree
                )
            ),
            node(tl(cons(5, emptylist)),
                node(cons(10, cons(9, cons(8, emptylist))), 
                    emptytree,
                    emptytree
                ),
                node(emptylist,
                    node(cons(9, emptylist),
                        emptytree,
                        emptytree
                    ),
─( 20:25:34 )─< command 16 >─────────────────────────────────────{ counter: 0 }─
utop # interp "
let t = node(emptylist ,
            node(cons(5, cons(2, cons(1, emptylist))), 
                emptytree ,
                node(emptylist , 
                    emptytree , 
                    emptytree
                )
            ),
            node(tl(cons(5, emptylist)),
                node(cons(10, cons(9, cons(8, emptylist))), 
                    emptytree,
                    emptytree
                ),
                node(emptylist,
                    node(cons(9, emptylist),
                        emptytree,
                        emptytree
                    ),
─( 20:25:34 )─< command 16 >─────────────────────────────────────{ counter: 0 }─
utop # interp "
let t = node(emptylist ,
            node(cons(5, cons(2, cons(1, emptylist))), 
                emptytree ,
                node(emptylist , 
                    emptytree , 
                    emptytree
                )
            ),
            node(tl(cons(5, emptylist)),
                node(cons(10, cons(9, cons(8, emptylist))), 
                    emptytree,
                    emptytree
                ),
                node(emptylist,
                    node(cons(9, emptylist),
                        emptytree,
                        emptytree
                    ),
                    emptytree
                )
            )
        )
 
in
caseT t of {
    emptytree -> 10,
    node(a, l, r) ->
        if empty?(a)
        then caseT l of {
                emptytree -> 21,
                node(b,ll,rr) -> if empty?(b)
                                 then 4
                                 else if zero?(hd(b))
                                      then 22
                                      else 99
            }
        else 5
}
";;
- : exp_val Proc.Ds.result = Ok (NumVal 99)
─( 20:25:43 )─< command 17 >─────────────────────────────────────{ counter: 0 }─

*)