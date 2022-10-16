open Ast
open Ds

(** [eval_expr e] evaluates expression [e] *)

let rec eval_expr : expr -> env -> exp_val result =
  fun e en ->
  match e with
  | Int (n)     -> return ( NumVal n )
  | Var(id) -> apply_env id en
  | Add(e1,e2) ->
    eval_expr e1 en >>= 
      int_of_numVal >>= fun n ->
    eval_expr e2 en 
      >>= int_of_numVal >>= fun m ->
    return (NumVal (n+m))   
  | Sub(e1,e2) ->
    eval_expr e1 en >>= int_of_numVal >>= fun n ->
    eval_expr e2 en >>= int_of_numVal >>= fun m ->
    return (NumVal(n-m))   
  | Mul(e1,e2) ->
    eval_expr e1 en >>= int_of_numVal >>= fun n ->
    eval_expr e2 en >>= int_of_numVal >>= fun m ->
    return (NumVal(n*m))   
  | Div(e1,e2) ->
    eval_expr e1 en >>= int_of_numVal >>= fun n ->
    eval_expr e2 en >>= int_of_numVal >>= fun m ->
    if m==0
    then error "Division by zero"
    else return (NumVal (n/m))
  | Abs(e) ->
    eval_expr e en >>= int_of_numVal >>= fun n ->
    return (NumVal(abs n))
  | IsZero(e) ->
      eval_expr e en >>=
      int_of_numVal >>= fun n ->
      return ( BoolVal ( n = 0))
  | ITE(e1,e2,e3) ->
    eval_expr e1 en >>= bool_of_boolVal >>= fun n ->
      if n 
      then eval_expr e2 en 
      else eval_expr e3 en
  | _ -> failwith "Not implemented yet!"


(** [parse s] parses string [s] into an ast *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_expr
  in c EmptyEnv



