type exp_val =
  | NumVal of int
  | BoolVal of bool

type env =
  | EmptyEnv
  | ExtendEnv of string * exp_val * env


type 'a result = Ok of 'a | Error of string

(* Environment Abstracted Result *)
type 'a ea_result = env -> 'a result

let return : 'a -> 'a ea_result =
  fun v ->
  fun env -> Ok v

let error : string -> 'a ea_result =
  fun s ->
  fun env -> Error s

let (>>=) : 'a ea_result -> ('a -> 'b ea_result) -> 'b ea_result =
  fun c f ->
    fun env ->
      match c env with
      | Error err -> Error err
      | Ok v -> f v env

let (>>+) : env ea_result -> 'a ea_result -> 'a ea_result =
  fun c d ->
    fun env ->
      match c env with
      | Error err -> Error err
      | Ok newenv -> d newenv

let run: 'a ea_result -> 'a result =
      fun c -> c EmptyEnv

  let empty_env: unit -> env =
  fun () -> EmptyEnv

let extend_env: string -> exp_val -> env ea_result =
  fun id v ->
    fun env -> Ok (ExtendEnv(id,v,env))

let rec apply_env: string -> exp_val ea_result =
  fun id env -> 
    match env with
    | EmptyEnv -> Error (id^" not found!")
    | ExtendEnv(v,ev,tail) ->
      if id=v
      then Ok ev
      else apply_env id tail

let int_of_numVal : exp_val -> int ea_result =
fun ev ->
match ev with
| NumVal n -> return n
| _ -> error " Expected a number!"

let bool_of_boolVal: exp_val -> bool ea_result =
fun ev ->
  match ev with
  | BoolVal n -> return n
  | _ -> error " Expected a bool!"