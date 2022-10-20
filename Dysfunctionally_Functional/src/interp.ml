open Ast
open Ds
open Printf
(** [eval_expr e] evaluates expression [e] *)

(* apply_clos ==> applying closure
The function apply_clos sets en to be the new current environment and then extends it with the
assignment of id to ev. Under this extended environment, it proceeds with the evaluation of the
body of the closure, namely e. *)
let rec apply_clos : string*Ast.expr*env -> exp_val -> exp_val ea_result =
  fun (id,e,en) ev ->
  (* return en >>+ *)
  extend_env id ev >>+
  eval_expr e
and 
  eval_expr : expr -> exp_val ea_result =
  fun e  ->
  match e with
  | Int (n) -> return ( NumVal n )
  | Var(id) -> apply_env id 
  | Add(e1,e2) ->
    eval_expr e1  >>= 
      int_of_numVal >>= fun n ->
    eval_expr e2  
      >>= int_of_numVal >>= fun m ->
    return (NumVal (n+m))   
  | Sub(e1,e2) ->
    eval_expr e1  >>= int_of_numVal >>= fun n ->
    eval_expr e2  >>= int_of_numVal >>= fun m ->
    return (NumVal(n-m))   
  | Mul(e1,e2) ->
    eval_expr e1  >>= int_of_numVal >>= fun n ->
    eval_expr e2  >>= int_of_numVal >>= fun m ->
    return (NumVal(n*m))   
  | Div(e1,e2) ->
    eval_expr e1  >>= int_of_numVal >>= fun n ->
    eval_expr e2  >>= int_of_numVal >>= fun m ->
    if m==0
    then error "Division by zero"
    else return (NumVal (n/m))
  | Abs(e) ->
    eval_expr e  >>= int_of_numVal >>= fun n ->
    return (NumVal(abs n))
  | IsZero(e) ->
      eval_expr e  >>=
      int_of_numVal >>= fun n ->
      return ( BoolVal ( n = 0))
  | ITE(e1,e2,e3) ->
    eval_expr e1  >>= bool_of_boolVal >>= fun n ->
      if n 
      then eval_expr e2  
      else eval_expr e3 
  | Let(id,def,body) ->
      eval_expr def >>= 
      extend_env id >>+
      eval_expr body
  
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
      eval_expr e2 >>= fun ev2 ->
        return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return (fst p) 
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return (snd p)
  | Unpair(id1,id2,e1,e2) ->
    eval_expr e1 >>= pair_of_pairVal >>= fun (x,y) ->
      extend_env id1 x >>+
      extend_env id2 y >>+
      eval_expr e2
  | Tuple(es) ->
    sequence (List.map eval_expr es) >>= fun evs ->
    return (TupleVal evs)
   | Untuple(ids,e1,e2) -> 
    eval_expr e1 >>=
    list_of_tupleVal >>= fun evs ->
      List.iter (printf "%s ") ids;
    if List.length ids<>List.length evs
      then error "untuple: mismatch"
    else extend_env_list ids evs >>+
      eval_expr e2
  | Record(fs) ->
    (* ----------- My implementation -----------
    let keys, values = List.split fs in
      if has_duplicates keys then error "Invalid record"
      else sequence (List.map eval_expr values) >>= fun evalues ->
        return (RecordVal(List.combine keys evalues))  
    ----------- My implementation -----------
    *)
    let keys, values = List.split fs in
    if has_duplicates keys then error "Invalid record"
    else
      sequence (List.map eval_expr values) >>= fun evalues ->
      return (RecordVal (List.combine keys evalues))
  | Proj(e,id) ->
    eval_expr e >>=
    fields_of_recordVal >>= fun fs ->
      (match List.assoc_opt id fs with
      | None -> error "Field not found!"
      | Some v -> return v) 

  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= 
    clos_of_procVal >>= fun clos ->
    eval_expr e2 >>= 
    apply_clos clos 
  | Letrec(id,par,e1,e2) ->
    extend_env_rec id par e1 >>+
    eval_expr e2 
  | Debug(_e) ->
      string_of_env >>= fun str ->
      print_endline str;
      error " Debug called "
  | Cons(e1, e2) -> failwith "implement me"
  | Hd(e1) ->  failwith "implement me"
  | Tl(e1) ->  failwith "implement me"
  | Empty(e1)  ->  eval_expr e1 >>=
        fun v1 -> if (isTreeVal v1) 
                      then  tree_of_TreeVal v1 >>= fun v3 -> return (BoolVal (v3 == Empty) )
                  else if(isList v1) 
                    then list_of_ListVal v1 >>= fun v3 -> return (BoolVal (v3 == [] ))
                  else error "Niether Tree nor List"

  | EmptyList    ->  return (ListVal [] )
  | EmptyTree ->  return (TreeVal(Empty))
  | Node(e1,lte,rte) ->  failwith "implement me"
  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->  failwith "implement me"
  | _ -> failwith "Not implemented yet!"


(** [parse s] parses string [s] into an ast *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_expr
  in run c



