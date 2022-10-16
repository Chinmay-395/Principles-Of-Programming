type exp_val =
  | NumVal of int
  | BoolVal of bool

type env =
  | EmptyEnv
  | ExtendEnv of string * exp_val * env

 
(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

let return : 'a -> 'a result =
  fun v ->
  Ok v

let error : string -> 'a result =
  fun s ->
  Error s

let (>>=) : 'a result -> ('a -> 'b result) -> 'b result =
  fun c f ->
  match c with
  | Error err -> Error err
  | Ok v -> f v

  let empty_env: unit -> env =
  fun () -> EmptyEnv

let extend_env: env -> string -> exp_val -> env =
  fun env id v -> ExtendEnv(id,v,env)

let rec apply_env: string -> env -> exp_val result =
  fun id env -> 
    match env with
    | EmptyEnv -> error (id^" not found!")
    | ExtendEnv(v,ev,tail) ->
      if id=v
      then return ev
      else apply_env id tail

let int_of_numVal : exp_val -> int result =
fun ev ->
match ev with
| NumVal n -> return n
| _ -> error " Expected a number!"

let bool_of_boolVal: exp_val -> bool result =
fun ev ->
  match ev with
  | BoolVal n -> return n
  | _ -> error " Expected a bool!"