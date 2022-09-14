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

let rec repeat n x = 
  match n with
  | 0 -> []
  | n -> x:: repeat x (n-1);;