let increment x:int = x + 1;;

Printf.printf "Incrementing 5 gives %d" (increment 5);;

(** the fibonacci series *)
let rec fib x:int =
  if x == 0 then 0
  else if x==1 then 1 
  else fib (x-1) + fib (x-2);;  

  
(** the power function *)
(* 
let rec lastValInList lst = 
match lst with 
| ((lenOfList lst) == 1) -> List.hd lst 
| h::t -> lastValInList t;;  


5
4
3
2
1
*)

