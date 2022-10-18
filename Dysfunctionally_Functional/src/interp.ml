open Ast
open Ds

(** [eval_expr e] evaluates expression [e] *)

let rec eval_expr : expr -> exp_val ea_result =
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
  (* | Abs(e) ->
    eval_expr e  >>= int_of_numVal >>= fun n ->
    return (NumVal(abs n)) *)
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
  (* | Debug(e) ->
      string_of_env >>= fun str ->
      print_endline str;
      error " Debug called " *)
  (* | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
      eval_expr e2 >>= fun ev2 ->
        return (PairVal(ev1,ev2)) *)
  | Tuple(es) ->
    sequence (List.map eval_expr es) >>= fun evs ->
    return (TupleVal evs)
  (* | Untuple(ids,e1,e2) ->
    eval_expr e1 >>=
    list_of_tupleVal >>= fun evs ->
    if List.length ids<>List.length evs
         then error "untuple: mismatch"
         else extend_env_list ids evs >>+
           eval_expr e2
  | Record(fs) ->
    let keys, values = List.split fs in
    if has_duplicates keys then error "Invalid record"
    else sequence (List.map eval_expr values) >>= fun evalues ->
      return (RecordVal(List.combine keys evalues)) *)
    (* let keys, values = List.split fs in
    if has_duplicates keys then error "Invalid record"
    else
      sequence (List.map eval_expr values) >>= fun evalues ->
      return (RecordVal (List.combine keys evalues)) *)
  (* | Proj(e,id) ->
    eval_expr e >>=
    fields_of_recordVal >>= fun fs ->
      (match List.assoc_opt id fs with
      | None -> error "Field not found!"
      | Some v -> return v) *)
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



