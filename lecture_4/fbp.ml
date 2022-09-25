(*
       Fruit Basket Processors
       22 Sep 2022
*)

type fruit = A | O | K
type fb = fruit list

let ex : fb = [A;A;O;O;O;O;K;K;A;A;K]

type 'a result = Ok of 'a | Error of string
                   
(* 
    Definition: A fruit basket processor is any expression whose type is:

       fb -> 'a result

    Givena fruit basket it processes it with one of two outcomes:
     1) Error
     2) Ok v, for some value v
*)

(* Some examples of fbp: counting fruit *)

let no_of_apples : fb -> int result =
  fun fb ->
  Ok (List.length (List.filter ((=)A) fb))

let no_of_oranges =
  fun fb ->
  Ok (List.length (List.filter ((=)O) fb))

let no_of_kiwis =
  fun fb ->
  Ok (List.length (List.filter ((=)K) fb))

(* Some more examples of fbp: has fruit *)

let has_apples : fb -> bool result =
  fun fb ->
  Ok (List.mem A fb)
    
(* Two more examples of fbps: remove an apple, orange *)

let rec rem_apples_helper fb =
  match fb with
  | [] -> []
  | A::t -> t
  | h::t -> h:: rem_apples_helper t
    
let remove_apple : fb -> fb result =
  fun fb ->
  if List.mem A fb
  then Ok (rem_apples_helper fb)
  else Error "no apples"

(** [silly] is not a fbp. However, [silly v] is: given a fb it just *)
(** ignores it returning [Ok v]. 
    One could say that [silly] is a fbp **generator**: given [v] is *)
(** generates a fbp [silly v] whose behavior is described above. *)

let silly : 'a -> fb -> 'a result =
  fun v ->
  fun fb ->
  Ok v


(** Two more examples of fbps.
    These examples are constructed by combining two fbps, say fbp1 and fbp2.
    Since (in principle) a fbp can return Error, after processing a
    fb with fbp1, we must check whether Error was returned before
    using fbp2. *)


let no_of_apples_and_oranges : fb -> int result =
  fun fb ->
  match no_of_apples fb with
  | Error s -> Error s
  | Ok na ->
    (match no_of_oranges fb with
     | Error s -> Error s
     | Ok no -> Ok (na+no))

let (>>=) : (fb -> 'a result) -> ('a -> fb -> 'b result) -> (fb -> 'b result) =
  fun fbp1 f ->
  fun fb ->
  match fbp1 fb with
  | Error s -> Error s
  | Ok v -> f v fb

let no_of_apples_and_oranges2 =
  no_of_apples >>= fun na ->
  no_of_oranges >>= fun no ->
  silly (na+no)
    
let has_apples_and_oranges : fb -> bool result =
  fun fb ->
  failwith "implement"
  
(** Two important observations on the last two examples:
    1. Note how, if fbp1 returns an Error, fbp2 is not used.
    2. Note how a copy of the fb is given to both fbp1 and fbp2. 
    3. Note how the second match makes use of the result of the first
    fbp1, namely [n]. 

    These observations are collected in the following helper function
    called bind. It essentially allows us to apply one fbp after
    another without having to deal with error propagation nor giving
    out copies of the fb to the fbps. *)
  
(* let (>>=) : (fb -> 'a result) -> ('a -> fb -> 'b result) ->  fb -> 'b result = *)


(** Using this helper function we can now rewrite the last two *)
(** examples as follows *)


(* let no_of_apples_and_oranges2 = *)
    
(* let has_apples_and_oranges2 = *)


(* 
silly/return, error, >>=  are called a error and reader monad combined 
*)



(*

sequence [no_of_apples;no_of_oranges;no_of_kiwis] fb1
===>
Ok [4;2;4]

sequence [apples_to_oranges_ratio;no_of_oranges] [A;A;O]
===>
Ok [2; 1]

sequence [apples_to_oranges_ratio;no_of_oranges] []
===>
Error

*) 


(* let rec sequence : (fb -> 'a result) list -> fb -> 'a list result = *)


(*
Informal description of how sequence works:

 sequence [fbp1;fbp2]
 ==>
 fbp1 >>= fun n ->
 fbp2 >>= fun m ->
 silly (n::m::[])

*)
