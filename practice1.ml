let increment x:int = x + 1;;

Printf.printf "Incrementing 5 gives %d" (increment 5);;

(** the fibonacci series *)
let rec fib x:int =
  if x == 0 then 0
  else if x==1 then 1 
  else fib (x-1) + fib (x-2);;  

let rec mem : 'a -> 'a list -> bool =
  fun ele list ->
    match list with
    | [] -> false
    | h::t -> if (h == ele) then true else mem ele t;;
    (* | _ -> failwith "Something went wrong";; *)


(** [rev l] returns the reverse of [l] *)
let rec rev l =
    match l with
    | [] -> []
    | h::t -> h @ rev t

(* Write a function list_enum that given a positive number n
returns the list [n;n-1;...;1;0] *)

let rec listEnum num =
  match num with
  | -1 -> []
  | n -> n :: listEnum (num-1);;

(* Write a function repeat that given an argument x and a
positive number n returns the list *)

let rec repeatFunc x n = 
  match n with
  | 0 -> []
  | n -> x:: repeatFunc x (n-1);;

(* Write a function stutter that given two positive numbers n
and m returns a new list of the form
[[n;n;...;n];[n-1;n-1;...;n-1];...;[0;0;...;0]] where each
nested list has m items 
For example, stutter 3 2 should return
[[3;3];[2;2];[1;1];[0;0]]
*)


let rec stutterFunc m n =
  match m with
  | 0 -> []
  | m -> m::repeatFunc (m-1) n;;

(* Write a function that multiplies all the numbers in a list *)
let rec multiplierFunc l =
  match l with
  | [] -> 1
  | h::t -> h * multiplierFunc t;;

(*  Functions that Construct Lists *)
let rec incr l =
match l with
| [] -> []
| x :: xs -> ( x +1):: incr xs

let rec stutter l =
match l with
[] -> []
| ( x :: xs ) -> x :: x ::( stutter xs )

(* Adjecent duplicates removal *)
let rec rad l =
  match l with
  | [] -> []
  | [x] -> [x]
  | h1::h2::t when h1=h2 -> rad (h2::t)
  | h1::h2::t -> h1:: rad (h2::t)


(* Define a function is_zero_list that given a list of numbers
returns a list of booleans indicating whether each number is 0
or not. *)
let rec is_zero_list l =
  match l with
  | [] -> []
  | (h::t) -> (h=0):: is_zero_list t;;

(** [last l] returns the last element of [l]. Should fail if [l] is empty *)
let rec last l =
  match l with
  | [] -> failwith "Its an empty list"
  | [x] -> x
  | h::t -> last t;;

(** [has_duplicates l] determines whether [l] has duplicates *)
let rec has_duplicates l =
  match l with 
  | [] -> false
  | h::t -> mem h t || has_duplicates t;;

(* (** [sublist l1 l2] determines whether [l1] is a sublist of [l2], *)
(** that is, every element of [l1] is in [l2]  *)
*)
let rec isSublist l1 l2 =
  match l1 with
  | [] -> true
  | h::t -> mem h l2 && isSublist t l2;;
  (* failwith "complete"  *)

(** [concatenate l1 l2] concatenates [l1] with [l2].  *)
(** Note: you may not use @  *)
let rec concatenate l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h :: concatenate t l2;;
  










  