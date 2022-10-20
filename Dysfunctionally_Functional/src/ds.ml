type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
type exp_val =
  | NumVal of int
  | BoolVal of bool
  | PairVal of exp_val*exp_val
  | TupleVal of exp_val list
  | RecordVal of ( string * exp_val ) list 
  | ProcVal of string*Ast.expr*env
  | UnitVal
  | ListVal of exp_val list
  | TreeVal of exp_val tree
and
 env =
  | EmptyEnv
  | ExtendEnv of string * exp_val * env
  | ExtendEnvRec of string*string*Ast.expr*env 


type 'a result = Ok of 'a | Error of string

(* Environment Abstracted Result *)
type 'a ea_result = env -> 'a result

let return (v:'a): 'a ea_result =
  fun _env -> Ok v

let error (s:string) : 'a ea_result =
  fun _env -> Error s

let (>>=) (c:'a ea_result) (f: 'a -> 'b ea_result) : 'b ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:env ea_result) (d:'a ea_result): 'a ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run (c:'a ea_result) : 'a result =
  c EmptyEnv

let lookup : env ea_result = fun env ->
  Ok env

let rec sequence l =
  match l with
  | [] -> return []
  | h::t ->
    h >>= fun ev ->
    sequence t >>= fun evs ->
    return (ev::evs)

let rec mem : 'a -> 'a list -> bool =
  fun e l ->
  match l with
  | [] -> false
  | h::t -> (e=h) || mem e t
              
let rec has_duplicates : 'a list -> bool =
  fun l ->
  match l with
  | [] -> false
  | h::t -> mem h t || has_duplicates t

let maperFunc (f:'a -> 'b ea_result) (vs:'a list) : ('b list) ea_result = sequence (List.map f vs)
(* Operations on environments *)

let empty_env : unit -> env ea_result =
  fun () -> return EmptyEnv

let extend_env : string -> exp_val -> env ea_result =
  fun id v ->
    fun env -> Ok (ExtendEnv(id,v,env))

let rec apply_env : string -> exp_val ea_result =
  fun id env -> 
    match env with
    | EmptyEnv -> Error (id^" not found!")
    | ExtendEnv(v,ev,tail) ->
      if id=v
      then Ok ev
      else apply_env id tail
    | ExtendEnvRec(v,par,body,tail) ->
    if id=v
    then Ok (ProcVal (par,body,env))
    else apply_env id tail

let rec string_of_list_of_strings = function
  | [] -> ""
  | [id] -> id
  | id::ids -> id ^ "," ^ string_of_list_of_strings ids

let isTreeVal = function
  
  | TreeVal(_) -> true
  | _ -> false 

let isList = function
  | ListVal(_) -> true
  | _ -> false

(* operations on expressed values *)
let int_of_numVal : exp_val -> int ea_result =
fun ev ->
match ev with
| NumVal n -> return n
| _ -> error " Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =
fun ev ->
  match ev with
  | BoolVal n -> return n
  | _ -> error " Expected a bool!"

let list_of_tupleVal : exp_val -> (exp_val list)  ea_result =  function
  |  TupleVal l -> return l
  | _ -> error "Expected a tuple!"
           
 let pair_of_pairVal : exp_val -> (exp_val*exp_val) ea_result =  function
  |  PairVal(ev1,ev2) -> return (ev1,ev2)
  | _ -> error "Expected a pair!"

let fields_of_recordVal : exp_val -> ((string*exp_val) list) ea_result = function
  | RecordVal(fs) -> return fs
  | _ -> error "Expected a record!"

let list_of_ListVal : exp_val -> (exp_val list) ea_result = function
  | ListVal(fs) -> return fs
  | _ -> error "Expected a list!"

let tree_of_TreeVal = function
  | TreeVal fs -> return fs
  | _ -> error "Expected a tree!"


let rec extend_env_list_helper =
  fun ids evs en ->
  match ids,evs with
  | [],[] -> en
  | id::idt,ev::evt ->
    ExtendEnv(id,ev,extend_env_list_helper idt evt en)
  | _,_ -> failwith
             "extend_env_list_helper: ids and evs have different sizes"
  
let extend_env_list =
  fun ids evs ->
  fun en ->
  Ok (extend_env_list_helper ids evs en)

let extend_env_rec : string -> string -> Ast.expr -> env ea_result =
  fun id par body env  ->
    Ok (ExtendEnvRec(id,par,body,env))

let lookup_env : env ea_result =
  fun env ->
  Ok env
let clos_of_procVal : exp_val -> (string*Ast.expr*env) ea_result =
  fun ev ->
  match ev with
  | ProcVal(id,body,en) -> return (id,body,en)
  | _ -> error "Expected a closure!"

let rec string_of_expval = function
  | NumVal n -> " NumVal " ^ string_of_int n
  | BoolVal b -> " BoolVal " ^ string_of_bool b
  | PairVal (ev1,ev2) -> "PairVal "^string_of_expval ev1
                         ^","^ string_of_expval ev2
  | TupleVal(evs) ->  "Tuple (" ^ string_of_list_of_strings (List.map
                                                   string_of_expval
                                                   evs)  ^ ")" 
  |RecordVal(fs) -> "RecordVal("^ String.concat "," 
      (List.map (fun (n,ev) ->
      n^"="^string_of_expval ev) fs) ^")"
  | ProcVal (id,body,env) -> "ProcVal ("^ id ^","^Ast.string_of_expr
                               body^","^ String.concat ",\n" (string_of_env' [] env)^")"
  | ListVal(_evs) -> "ListVal"
  | TreeVal(_t) -> "TreeVal"
  | UnitVal  -> "UnitVal"
and 
  string_of_env' ac = function
  | EmptyEnv -> ac
  | ExtendEnv (id,v,env ) -> string_of_env'(( id ^ " := " ^ string_of_expval v ) :: ac) env
  | ExtendEnvRec(id,param,body,env) -> string_of_env'
  ((id^":=Rec("^param^","^Ast.string_of_expr body^")")::ac) env
let string_of_env : string ea_result =
  fun env ->
  match env with
  | EmptyEnv -> Ok " >> Environment : \ nEmpty "
  | _ -> Ok ( " >> Environment : \ n " ^ String . concat " ,\ n " ( string_of_env' [] env ))