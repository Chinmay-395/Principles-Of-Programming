(* Name: Chinmay Dali & Tanay Shah *)
open Ast
open Ds

let g_store = Store.empty_store 20 (NumVal 0)


      
let rec addIds fs evs =
  match fs,evs with
  | [],[] -> []
  | (id,(is_mutable,_))::t1, v::t2 -> (id,(is_mutable,v)):: addIds t1 t2
  | _,_ -> failwith "error: lists have different sizes"

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
  | Int(n) -> return @@ NumVal n
  | Var(id) -> apply_env id
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
  | Let(v,def,body) ->
    eval_expr def >>= 
    extend_env v >>+
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
  | IsEqual(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (BoolVal (n1=n2))
  | IsGT(e1,e2) ->
        eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (BoolVal (n1>n2))
  | IsLT(e1,e2) ->
        eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (BoolVal (n1<n2))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return @@ PairVal(ev1,ev2)
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ fst p 
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ snd p
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Letrec(id,par,e,target) ->
    extend_env_rec id par e >>+
    eval_expr target
  | NewRef(e) ->
    eval_expr e >>= fun ev ->
    return @@ RefVal (Store.new_ref g_store ev)
  | DeRef(e) ->
    eval_expr e >>=
    int_of_refVal >>= 
    Store.deref g_store
  | SetRef(e1,e2) ->
    eval_expr e1 >>=
    int_of_refVal >>= fun l ->
    eval_expr e2 >>= 
    Store.set_ref g_store l >>= fun _ ->
    return UnitVal
  | BeginEnd([]) ->
    return UnitVal
  | BeginEnd(es) ->
    sequence (List.map eval_expr es) >>= fun vs ->
    return (List.hd (List.rev vs))
  | Record(fs) ->
    sequence (List.map process_field fs) >>= fun evs ->
    return (RecordVal (addIds fs evs))
  | Proj(e,id) ->
    eval_expr e >>=
      fields_of_recordVal >>= fun fs -> 
    
        ( 
        match List.assoc_opt id fs with 
        | Some (true, ev) -> int_of_refVal ev >>= fun i ->
             Store.deref g_store i >>= fun v -> return v
        |Some(false,ev) -> return ev
        | _ -> error "Failed not found"
      )

  | SetField(e1,id,e2) ->
    eval_expr e1 >>=
  fields_of_recordVal >>= fun x1  ->
    eval_expr e2 >>= fun x2 -> 
       let second = List.assoc_opt id x1 in
   (match second with
   | None -> error "Field not found"
   | Some(true, RefVal a) -> (Store.set_ref g_store a x2) >>= 
      fun _final -> return UnitVal 
   | _ -> error "Field not mutable")
    
  | IsNumber(e) ->
    eval_expr e >>= is_numVal >>= fun number -> return (BoolVal number)
  | Unit -> return UnitVal
  | Debug(_e) ->
    string_of_env >>= fun str_env ->
    let str_store = Store.string_of_store string_of_expval g_store 
    in (print_endline (str_env^"\n"^str_store);
    error "Reached breakpoint")
  | _ -> error ("Not implemented: "^string_of_expr e)
and
  process_field (_id,(is_mutable,e)) =
  eval_expr e >>= fun ev ->
  if is_mutable
  then return (RefVal (Store.new_ref g_store ev))
  else return ev

             
(* Parse a string into an ast *)

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf 


(* Interpret an expression *)
let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_expr
  in run c


let read_file (filename:string) : string = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    "" (* never reaches this line *)
  with End_of_file ->
    close_in chan;
    String.concat "" (List.rev !lines)

(* Parse an expression read from a file with optional extension .sool *)
let parsef (s:string) : expr = 
  let s = String.trim s      (* remove leading and trailing spaces *)
  in let file_name =    (* allow rec to be optional *)
       match String.index_opt s '.' with None -> s^".sool" | _ -> s
  in
  parse @@ read_file file_name


(* Interpret an expression read from a file with optional extension .sool *)
let interpf (s:string) : exp_val result = 
  let s = String.trim s      (* remove leading and trailing spaces *)
  in let file_name =    (* allow rec to be optional *)
       match String.index_opt s '.' with None -> s^".exr" | _ -> s
  in
  interp @@ read_file file_name


(* Output
  
─( 22:19:19 )─< command 0 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # interpf "ll_add_front";;
>>Environment:
l1->{head <= RefVal (0); size <= RefVal (1)},
add_front->ProcVal(x,Proc(l,BeginEnd(Var l.head<={data=Var x;next=Var l.head};Var l.size<=Add(Var l.size,Int 1))),[l1->{head <= RefVal (0); size <= RefVal (1)}])
>>Store:
0->{data <= RefVal (4); next <= RefVal (5)},
1->NumVal 2,
2->NumVal 2,
3->NumVal 0,
4->NumVal 3,
5->{data <= RefVal (2); next <= RefVal (3)}
- : exp_val Explicit_refs.Ds.result = Error "Reached breakpoint"

─( 22:19:19 )─< command 1 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # interpf "ll_bump";;
>>Environment:
l1->{head <= RefVal (6); size <= RefVal (7)},
add_front->ProcVal(x,Proc(l,BeginEnd(Var l.head<={data=Var x;next=Var l.head};Var l.size<=Add(Var l.size,Int 1))),[l1->{head <= RefVal (6); size <= RefVal (7)}]),
bump_helper->Rec(node,IfThenElse(Number?(Var node),Int 0,BeginEnd(Var node.data<=Add(Var node.data,Int 1);App(Var bump_helper,Var node.next)))),
bump->ProcVal(ll,App(Var bump_helper,Var ll.head),[l1->{head <= RefVal (6); size <= RefVal (7)},add_front->ProcVal(x,Proc(l,BeginEnd(Var l.head<={data=Var x;next=Var l.head};Var l.size<=Add(Var l.size,Int 1))),[l1->{head <= RefVal (6); size <= RefVal (7)}]),bump_helper->Rec(node,IfThenElse(Number?(Var node),Int 0,BeginEnd(Var node.data<=Add(Var node.data,Int 1);App(Var bump_helper,Var node.next))))])
>>Store:
0->{data <= RefVal (4); next <= RefVal (5)},
1->NumVal 2,
2->NumVal 2,
3->NumVal 0,
4->NumVal 3,
5->{data <= RefVal (2); next <= RefVal (3)},
6->{data <= RefVal (10); next <= RefVal (11)},
7->NumVal 2,
8->NumVal 3,
9->NumVal 0,
10->NumVal 4,
11->{data <= RefVal (8); next <= RefVal (9)}
- : exp_val Explicit_refs.Ds.result = Error "Reached breakpoint"


─( 22:20:10 )─< command 2 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # interpf "ll_remove_first";;
>>Environment:
l1->{head <= RefVal (12); size <= RefVal (13)},
add_front->ProcVal(x,Proc(l,BeginEnd(Var l.head<={data=Var x;next=Var l.head};Var l.size<=Add(Var l.size,Int 1))),[l1->{head <= RefVal (12); size <= RefVal (13)}]),
remove_first->ProcVal(l,BeginEnd(Var l.head<=Var l.head.next;Var l.size<=Sub(Var l.size,Int 1)),[l1->{head <= RefVal (12); size <= RefVal (13)},add_front->ProcVal(x,Proc(l,BeginEnd(Var l.head<={data=Var x;next=Var l.head};Var l.size<=Add(Var l.size,Int 1))),[l1->{head <= RefVal (12); size <= RefVal (13)}])])
>>Store:
0->{data <= RefVal (4); next <= RefVal (5)},
1->NumVal 2,
2->NumVal 2,
3->NumVal 0,
4->NumVal 3,
5->{data <= RefVal (2); next <= RefVal (3)},
6->{data <= RefVal (10); next <= RefVal (11)},
7->NumVal 2,
8->NumVal 3,
9->NumVal 0,
10->NumVal 4,
11->{data <= RefVal (8); next <= RefVal (9)},
12->{data <= RefVal (16); next <= RefVal (17)},
13->NumVal 2,
14->NumVal 2,
15->NumVal 0,
16->NumVal 3,
17->{data <= RefVal (14); next <= RefVal (15)},
18->NumVal 4,
19->{data <= RefVal (16); next <= RefVal (17)}
- : exp_val Explicit_refs.Ds.result = Error "Reached breakpoint"

─( 22:20:55 )─< command 3 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # interpf "ll_remove_last";;
>>Environment:
l1->{head <= RefVal (20); size <= RefVal (21)},
add_front->ProcVal(x,Proc(l,BeginEnd(Var l.head<={data=Var x;next=Var l.head};Var l.size<=Add(Var l.size,Int 1))),[l1->{head <= RefVal (20); size <= RefVal (21)}]),
remove_last_helper->Rec(l,Proc(orig,IfThenElse(Number?(Var l.next.next),BeginEnd(Var l.next<=Int 0;Var orig.size<=Sub(Var orig.size,Int 1)),App(App(Var remove_last_helper,Var l.next),Var orig)))),
remove_last->ProcVal(list,IfThenElse(Number?(Var list.head.next),BeginEnd(Var list.head<=Var list.head.next;Var list.size<=Sub(Var list.size,Int 1)),App(App(Var remove_last_helper,Var list.head),Var list)),[l1->{head <= RefVal (20); size <= RefVal (21)},add_front->ProcVal(x,Proc(l,BeginEnd(Var l.head<={data=Var x;next=Var l.head};Var l.size<=Add(Var l.size,Int 1))),[l1->{head <= RefVal (20); size <= RefVal (21)}]),remove_last_helper->Rec(l,Proc(orig,IfThenElse(Number?(Var l.next.next),BeginEnd(Var l.next<=Int 0;Var orig.size<=Sub(Var orig.size,Int 1)),App(App(Var remove_last_helper,Var l.next),Var orig))))])
>>Store:
0->{data <= RefVal (4); next <= RefVal (5)},
1->NumVal 2,
2->NumVal 2,
3->NumVal 0,
4->NumVal 3,
5->{data <= RefVal (2); next <= RefVal (3)},
6->{data <= RefVal (10); next <= RefVal (11)},
7->NumVal 2,
8->NumVal 3,
9->NumVal 0,
10->NumVal 4,
11->{data <= RefVal (8); next <= RefVal (9)},
12->{data <= RefVal (16); next <= RefVal (17)},
13->NumVal 2,
14->NumVal 2,
15->NumVal 0,
16->NumVal 3,
17->{data <= RefVal (14); next <= RefVal (15)},
18->NumVal 4,
19->{data <= RefVal (16); next <= RefVal (17)},
20->{data <= RefVal (26); next <= RefVal (27)},
21->NumVal 2,
22->NumVal 2,
23->NumVal 0,
24->NumVal 3,
25->NumVal 0,
26->NumVal 4,
27->{data <= RefVal (24); next <= RefVal (25)}
- : exp_val Explicit_refs.Ds.result = Error "Reached breakpoint"


─( 22:21:22 )─< command 4 >──────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
utop # interpf "ll_add_last";;
>>Environment:
l1->{head <= RefVal (28); size <= RefVal (29)},
add_front->ProcVal(a,Proc(b,BeginEnd(Var b.head<={data=Var a;next=Var b.head};Var b.size<=Add(Var b.size,Int 1))),[l1->{head <= RefVal (28); size <= RefVal (29)}]),
add_last_helper->Rec(l,Proc(x,Proc(orig,IfThenElse(Number?(Var l.next),BeginEnd(Var l.next<={data=Var x;next=Int 0};Var orig.size<=Add(Var orig.size,Int 1)),App(App(App(Var add_last_helper,Var l.next),Var x),Var orig))))),
add_last->ProcVal(list,Proc(v,IfThenElse(Number?(Var list.head),App(App(Var add_front,Var v),Var list),App(App(App(Var add_last_helper,Var list.head),Var v),Var list))),[l1->{head <= RefVal (28); size <= RefVal (29)},add_front->ProcVal(a,Proc(b,BeginEnd(Var b.head<={data=Var a;next=Var b.head};Var b.size<=Add(Var b.size,Int 1))),[l1->{head <= RefVal (28); size <= RefVal (29)}]),add_last_helper->Rec(l,Proc(x,Proc(orig,IfThenElse(Number?(Var l.next),BeginEnd(Var l.next<={data=Var x;next=Int 0};Var orig.size<=Add(Var orig.size,Int 1)),App(App(App(Var add_last_helper,Var l.next),Var x),Var orig)))))])
>>Store:
0->{data <= RefVal (4); next <= RefVal (5)},
1->NumVal 2,
2->NumVal 2,
3->NumVal 0,
4->NumVal 3,
5->{data <= RefVal (2); next <= RefVal (3)},
6->{data <= RefVal (10); next <= RefVal (11)},
7->NumVal 2,
8->NumVal 3,
9->NumVal 0,
10->NumVal 4,
11->{data <= RefVal (8); next <= RefVal (9)},
12->{data <= RefVal (16); next <= RefVal (17)},
13->NumVal 2,
14->NumVal 2,
15->NumVal 0,
16->NumVal 3,
17->{data <= RefVal (14); next <= RefVal (15)},
18->NumVal 4,
19->{data <= RefVal (16); next <= RefVal (17)},
20->{data <= RefVal (26); next <= RefVal (27)},
21->NumVal 2,
22->NumVal 2,
23->NumVal 0,
24->NumVal 3,
25->NumVal 0,
26->NumVal 4,
27->{data <= RefVal (24); next <= RefVal (25)},
28->{data <= RefVal (30); next <= RefVal (31)},
29->NumVal 3,
30->NumVal 2,
31->{data <= RefVal (32); next <= RefVal (33)},
32->NumVal 3,
33->{data <= RefVal (34); next <= RefVal (35)},
34->NumVal 4,
35->NumVal 0
- : exp_val Explicit_refs.Ds.result = Error "Reached breakpoint"
*)