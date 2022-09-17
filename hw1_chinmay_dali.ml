type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]

let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

let rec map : ('a ->'b) ->  'a list -> 'b list =
  fun f l ->
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let mirror_func num =
  match num with 
  | 0 -> 0
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | 1 -> 1
  | _ -> failwith "Incorrect input"

let rotate_90_func num =
  match num with 
  | 0 -> 0
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | 1 -> 1
  | _ -> failwith "Incorrect input"

let mirror_image l =
  map mirror_func l;;

let rotate_90_letter l = map rotate_90_func l;;

let rec rotate_90_word l = 
  match l with
  | [] -> []
  | h::t -> rotate_90_letter h :: rotate_90_word t;;

let rec repeat: int->'a->'a list = 
  fun n x ->
  if n==0 then [] else x:: repeat (n-1) x;;



(* Pantograph question *)

(* Pantograph With map *)

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

let pantograph_nm n list =
    let rec prepend n acc x =
      if n = 0 then acc else prepend (n-1) (x :: acc) x in
        let rec aux acc = 
          function
          | [] -> acc
          | h :: t -> if (h<>0 && h<>1) then aux (prepend n acc h) t else aux (prepend 1 acc h) t
          in aux [] (List.rev list);;


(* Pantograph with fold *)
(* let pantograph_f n list = List.fold_left (fun x -> 
      if (x<>0 && x<>1) 
      then repeat n x 
      else repeat 1 x
    ) [] list;; *)

(* Coverage *)
(* The coverage will have a helper function which starts the list of the
   [(0,0)] and uses that to get the next possible co-ordinate *)
let helper acc direction =
     if (direction=2) then (fst(List.hd acc),snd(List.hd acc)+1) 
else if (direction=3) then (fst(List.hd acc)+1,snd(List.hd acc))
else if (direction=4) then (fst(List.hd acc),snd(List.hd acc)-1)
else if (direction=5) then (fst(List.hd acc)-1,snd(List.hd acc))
else if (direction=1||direction=0) then (fst(List.hd acc),snd(List.hd acc))
else failwith "Wrong input";;

let rec coverage': (int*int) list -> int list -> (int*int) list = 
  fun list_of_tuples list_of_direc -> 
  match list_of_direc with
  | [] -> []
  | h::t -> helper list_of_tuples h :: coverage' list_of_tuples t;;

(* let coverage initial_coordinate list_of_directions =  *)


let rec coverage: int*int -> int list -> (int*int) list =
  fun co_oridinate list_of_directions ->
    match co_oridinate,list_of_directions with
    | (x_coordinate,y_cooridnate),[] -> [(x_coordinate,y_cooridnate)]
    | (x_coordinate,y_cooridnate),h::t -> 
        if h==0 
          then (x_coordinate,y_cooridnate)::coverage (x_coordinate,y_cooridnate) t else
        if h==1 
          then coverage (x_coordinate,y_cooridnate) t else
        if h==2 
          then (x_coordinate,y_cooridnate+1)::coverage (x_coordinate,y_cooridnate+1) t else
        if h==3 
          then (x_coordinate+1,y_cooridnate)::coverage (x_coordinate+1,y_cooridnate) t else
        if h==4
          then (x_coordinate,y_cooridnate-1)::coverage (x_coordinate,y_cooridnate-1) t else
        if h==5
          then (x_coordinate-1,y_cooridnate)::coverage (x_coordinate-1,y_cooridnate) t else
        failwith "Incorrect Input";;

(* 7 Compress *)
let compress: int list -> (int*int) list =
fun l ->
  let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (x, count+1) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((a,count+1) :: acc) t in
    List.rev (aux 0 [] l);;

(* 8. Uncompress 
   8.1: uncompress_m 
   8.2: uncompress_f *)

let rec uncompress : (int*int) list -> int list =
  fun list ->
    match list with
    | [] -> []
    | (a,b)::t -> repeat b a @ uncompress t

(* let rec uncompress_m *)
(* let rec uncompress_f:  (int*int) list -> int list = List.fold_right (@)  *)

(* 9. Optimize  
* assuming the pen is initially in the up position   
*)

let rec helperFuncOptimize l = 
    match l with
    |[] -> []
    |h :: [] -> h :: []
    |h :: next :: t -> 
        if(h != next) then h :: helperFuncOptimize(next :: t)
        else 
            if(h != 0 && h != 1) then h :: next :: helperFuncOptimize(t)
            else helperFuncOptimize(next :: t);;

(** This function remove first number '1' in list if exist *)            
let remove_first_1 l = 
    match l with 
    |[] -> []
    |h::t -> 
        if(h = 1) then t
        else h::t;;

let optimize l = List.tl(helperFuncOptimize(l));;
