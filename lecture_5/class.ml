let rec insert_at x n l = 
    match l with
    | [] -> [x]
    | h :: t -> if n = 0 then x :: l else h :: insert_at x (n-1) t;;


let rec functionName list_val start endVal i newList =
match list_val with
| [] -> []
| h::t -> if(i>=start && i<=endVal) 
            then h::newList 
          else functionName t (start-1) (endVal-1) (i+1) newList;;

let rec functionName list_val start endVal i newList =
match list_val with
| [] -> []
| h::t -> List.mapi (fun i x -> if(i>=start && i<=endVal) then x::newList else l) list_val


(* let rec functionName list_val start endVal i newList =
 if(start>=i && start<=endVal) then h::newList else functionName t (start) endVal (i+1) newList;; *)



let rec functionName list_val start endVal i newList =
 if(i<List.length(list_val))
  then 
    if(i>=start)
      then if(i<=endVal)
        then functionName list_val (start) (endVal) (i+1) newList
else newList