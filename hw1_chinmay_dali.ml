type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]

let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(* 1. Mirror image of Mini-Logo *)
let mirror_func: int->int =
  fun num ->
  match num with 
  | 0 -> 0
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | 1 -> 1
  | _ -> failwith "Incorrect input"


let mirror_image: int list -> int list  =
  fun l -> List.map mirror_func l;;

(* 2. rotate_90_letter *)
let rotate_90_func: int->int =
  fun num ->
  match num with 
  | 0 -> 0
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | 1 -> 1
  | _ -> failwith "Incorrect input"
let rotate_90_letter l = List.map rotate_90_func l;;
(* 3. rotate _90_word *)
let rec rotate_90_word: int list list -> int list list =
  fun l -> 
  match l with
  | [] -> []
  | h::t -> rotate_90_letter h :: rotate_90_word t;;

(* 4. repeat *)
let rec repeat: int->'a->'a list = 
  fun n x ->
  if n==0 then [] else x:: repeat (n-1) x;;



(* 5. Pantograph *)

(* 5.1 Pantograph With map *)

let rec flatten : 'a list list -> 'a list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> h @ flatten t

let pantograph n list = flatten (
    List.map (fun x -> 
      if (x<>0 && x<>1) 
      then repeat n x 
      else repeat 1 x
    ) list
  );;
(* 5.2 pantograph without using map *)
let pantograph_nm n list =
    let rec prepend n acc x =
      if n = 0 then acc else prepend (n-1) (x :: acc) x in
        let rec aux acc = 
          function
          | [] -> acc
          | h :: t -> if (h<>0 && h<>1) then aux (prepend n acc h) t else aux (prepend 1 acc h) t
          in aux [] (List.rev list);;


(* 5.3 Pantograph with fold *)
(* ('a->'b->'c->'c) -> 'c -> 'b -> 'a list -> 'c *)
let rec foldr  =
  fun f a n l ->
  match l with
  | [] -> a
  | h::t -> f h n (foldr f a n t);;

let pantograph_f = foldr (fun x n r -> (if (x<>0 && x<>1) 
      then repeat n x 
      else repeat 1 x) @ r) [];;

(* 6. Coverage *)
let directions (x, y) num = 
    match num with 
    |2 -> (x, y+1)
    |3 -> (x+1, y)
    |4 -> (x, y-1)
    |5 -> (x-1, y)
    |_ -> (x,y);;

let rec coverage: (int*int) -> int list -> (int*int) list  = 
  fun (x, y) l ->
    match l with 
    |[] -> []
    |h::t -> directions (x,y) h :: coverage(directions (x, y) h) t;;

(* 7 Compress *)
let compress: int list -> (int*int) list =
fun l ->
  let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (x, count+1) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((a,count+1) :: acc) t in
    List.rev (aux 0 [] l);;

(* 8. Uncompress *)

(* 8.1 uncompress *)
let rec uncompress : (int*int) list -> int list =
  fun list ->
    match list with
    | [] -> []
    | (a,b)::t -> repeat b a @ uncompress t
(* 8.2 uncompress with map  *)
let rec uncompress_m list_of_tuples = flatten(List.map (fun tuple -> repeat (snd tuple) (fst tuple)) list_of_tuples);;
  (* 8.3 uncompress with fold *)
(* let rec uncompress_f:  (int*int) list -> int list = foldr (fun x n r -> (if (x<>0 && x<>1) 
      then repeat n x 
      else repeat 1 x) @ r) [];;  *)

(* 9. Optimize  *)

let rec helperFuncOptimize l = 
    match l with
    |[] -> []
    |h :: [] -> h :: []
    |h :: next :: t -> 
        if(h != next) then h :: helperFuncOptimize(next :: t)
        else 
            if(h != 0 && h != 1) then h :: next :: helperFuncOptimize(t)
            else helperFuncOptimize(next :: t);;

let optimize l = List.tl(helperFuncOptimize(l));;
