(* Simple examples of recursion *)
(* 
   Lecture 1 
   1 Sep 2022
*)

(* recursion on numbers *)

let rec fact n =
  match n with
  | 0 -> 1
  | m when m>0 -> m * fact (m-1)
  | _ -> failwith "fact: negative input"
     
(** [fact n] returns the factorial of [n]
    Precondition: [n] is positive *)
let rec fact n =
  match n with
  | 0 -> 1
  | m -> m * fact (m-1)

let rec repeat : int -> 'a -> 'a list =
  fun n e ->
  match n with
  | 0 -> []
  | m -> e :: repeat (m-1) e
 

(* recursion on lists *)

let rec size : 'a list -> int =
  fun l -> match l with
  | [] -> 0
  | h::t -> 1 + size t

let rec sum : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + sum t

let rec f l =
  match l with
  | [] -> []
  | h::t -> h::h:: f t

(* take 2 [1;2;3] => [1;2]
   take 10 [1;2;3] => [1;2;3]
   take 0 [1;2;3] => []
*)
let rec take : int -> 'a list -> 'a list =
  fun n l ->
  match n,l with
  | 0,_ -> []
  | _,[] -> []
  | m,h::t -> h::take (m-1) t

(* rad [1;2;2;2;3;1;4;4;4] =>
      [1;2;3;1;4] *)
                
let rec rad l =
  match l with
  | [] -> []
  | [x] -> [x]
  | h1::h2::t when h1=h2 -> rad (h2::t)
  | h1::h2::t -> h1:: rad (h2::t)
     
