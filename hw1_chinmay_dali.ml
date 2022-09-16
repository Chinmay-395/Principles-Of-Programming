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

(* Coverage *)
(* The coverage will have a helper function which starts the list of the
   [(0,0)] and uses that to get the next possible co-ordinate *)
