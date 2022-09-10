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
