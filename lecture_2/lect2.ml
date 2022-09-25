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
  fun l ->
  match l with
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



let rec mem : 'a -> 'a list -> bool =
  fun e l ->
  match l with
  | [] -> false
  | h::t -> (e=h) || mem e t
              
let rec has_duplicates : 'a list -> bool =
  fun l ->
  match l with
  | [] -> false
  | h::t -> mem h t || has_duplicates t

(** [sublist l1 l2] determines whether [l1] is a sublist of [l2]. We say [l1] is a sublist of [l2] if every element of [l1] is an element of [l2] *)
let rec sublist l1 l2 =
  match l1 with
  | [] -> true
  | h::t -> mem h l2 && sublist t l2

let rec rev l =
  match l with
  | [] -> []
  | h::t -> rev t @ [h]

let rec rev' l a =
  match l with
  | [] -> a
  | h::t -> rev' t (h::a)

(*
   using tail-recursion (memoization)

   rev' [1;2;3] []
   ==>
   rev' [2;3] 1::[]
   ==>
   rev' [3] 2::1::[]
   ==>
   rev' []  3::2::1::[]
   ==>
   3::2::1::[]

*)
let rec last : 'a list -> 'a =
  fun l ->
  match l with
  | [] -> failwith "last: empty list"
  | [x] -> x
  | h::t -> last t

let rec concat l1 l2 =
  match l1 with
  | [] -> l2
  | h::t ->  h :: concat t l2

(* Well-known higher-order function schemes *)


(* Map *)

let succ i = i+1
let upper c = Char.uppercase_ascii c
let isz i = i=0
            
let rec succl : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> succ h :: succl t

let rec upperl : char list -> char list  =
  fun l ->
  match l with
  | [] -> []
  | h::t -> upper h :: upperl t
              
let rec is_zerol : int list -> bool list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> isz h :: is_zerol t

let rec map : ('a ->'b) ->  'a list -> 'b list =
  fun f l ->
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let succl' l = map succ l
let upperl' l = map upper l
let is_zerol' l = map isz l

(* Exercise:
    What is the type of 

     map map
*)


(* Filter *)

let is_pos i = i>0
let is_upper c = Char.uppercase_ascii c =c
let isne l = l<>[]
                
let rec gtz : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_pos h
    then h::gtz t
    else gtz t
        
let rec uppercase : char list -> char list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_upper h
    then h::uppercase t
    else uppercase t
      
let rec non_empty : 'a list list -> 'a list list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if isne h
    then h::non_empty t
    else non_empty t

let rec filter : ('a -> bool)  -> 'a list -> 'a list =
  fun p l ->
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h::filter p t
    else filter p t
        
let gtz' l = filter is_pos l
let uppercase' l = filter is_upper l
let non_empty' l = filter isne l

(* fold *)

let rec suml : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + suml t
              
let rec andl : bool list -> bool =
  fun l->
  match l with
  | [] -> true
  | h::t -> h && andl t

let rec flatten : 'a list list -> 'a list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> h @ flatten t

let rec foldr : ('a -> 'b -> 'b)  -> 'b -> 'a list -> 'b =
  fun f a l ->
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)

let suml' l = foldr (+) 0 l
let andl' l = foldr (&&) true l
let flatten' l = foldr (@) [] l

(* foldr f a [x;y;z]
   ==>
   f x (f y (f z a)))
*)

(* foldl f a [x;y;z]
   ==>
   f (f (f a x) y) z
*)

let rec foldl f a l =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t

(* Algebraic Data Types *)


type dot = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let is_weekend : dot -> bool =
  fun d ->
  match d with
  | Sat -> true
  | Sun -> true
  | _ -> false

(* Illustrates constructors with arguments *)
type flavor = Van | Cho | Str
type ic = Cone of flavor | Cup of flavor*flavor | Bucket of flavor list

let ic1 = Cone(Van)
let ic2 = Cup(Van,Cho)
let ic3 = Bucket [Van;Cho;Van]

let price ic =
  match ic with
  | Cone(_) -> 1
  | Cup(_,_) -> 2
  | Bucket _ -> 5

let is_boring ic =
  match ic with
  | Cone(Van) -> true
  | Cup(Van,Van) -> true
  | Bucket l -> List.for_all ((=)Van) l
  | _ -> false

(* Polymorphic ADTs *)

type ('a,'b) either = Left of 'a | Right of 'b

type 'a option = Some of 'a | None
                 
let rec find : 'a -> ('a*'b) list -> 'b option =
  fun k l ->
  match l with
  | [] -> None
  | (k',v')::t ->
    if k=k'
    then Some v'
    else find k t

(* Binary trees *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

(*
       33
     /    \ 
   12     57
    \    /
     7  44
 *)

let t1 : int bt =
  Node(33,
       Node(12,
            Empty,
            Node(7,Empty,Empty)),
       Node(57,
            Node(44,Empty,Empty),
            Empty))

(*
          33
        /    \ 
      20     57
     /  \    / \
  10   25  44   91
 *)

let t2 : int bt =
  Node(33,
       Node(20,
            Node(10,Empty,Empty),
            Node(25,Empty,Empty)),
       Node(40,
            Node(35,Empty,Empty),
            Node(45,Empty,Empty)))

let t3: int bt = Node(13,
                  Node(10,
                    Node(7,
                      Empty,
                      Node(9,
                        Node(8,Empty,Empty),
                          Empty)
                    ),
                  Node(12,Empty,Empty)),
                  Node(15,
                    Node(14,Empty,Empty),
                    Node(17,Node(16,Empty,Empty),Empty))
                )
                      
let rec sizet t =
  match t with
  | Empty -> 0
  | Node(_,lt,rt) -> 1 + sizet lt + sizet rt

let rec sumt t =
  match t with
  | Empty -> 0
  | Node(d,lt,rt) -> d + sumt lt + sumt rt

let rec mapt f t =
  match t with
  | Empty -> Empty
  | Node(d,lt,rt) -> Node(f d,mapt f lt, mapt f rt)

let rec height t =
  match t with
  | Empty -> 0
  | Node(_,lt,rt) -> 1 + max (height lt) (height rt)

(*
       33
     /    \ 
   12     57
    \    /
     7  44

[33;12;7;57;44]
 *)
                       
let rec pre t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) -> [d] @ pre lt @ pre rt

let rec ino t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) ->  ino lt @ [d] @ ino rt

let rec pos t =
  match t with
  | Empty -> []
  | Node(d,lt,rt) ->  pos lt @ pos rt @ [d]

let rec foldt f a t =
  match t with
  | Empty -> a
  | Node(d,lt,rt) -> f d (foldt f a lt) (foldt f a rt)

(* let rec prune t l =
  match t with
  | Empty -> 
  failwith "implement" *)

(* Returns the list of data items held in leaves 
   Eg. leaves t1 ==> [7;44] *)
let rec leaves t =
  match t with
  | Empty -> []
  |Node(d,lt,rt) -> 
    if (lt=Empty && rt=Empty) 
      then [d] @ leaves lt @ leaves rt 
    else [] @ leaves lt @ leaves rt 
  (* failwith "implement" *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

let rec helper :int bt->int-> int->bool=
fun t low high ->
match t with
| Empty -> true
| Node(d,lt,rt) -> if ((d < low) && (d>high))
                    then (helper lt low d && helper rt d high)
                    else false

let rec is_bst t = helper t Int.min_int Int.max_int;;
  (* match t with 
  | Empty -> true
  | Node(d,lt,rt) -> 
  failwith "implement" *)

let rec find_bst t k =
  match t with 
  | Empty -> false
  | Node(d,lt,rt) -> if (d=k) then true else find_bst lt k && find_bst rt k;;

(** fails if key [k] already in [t] *)
let rec add_bst t k =
  failwith "implement"
    
let rec remove_bst t k =
  failwith "implement"


(*
       33
     /    \ 
   12     57
    \    /
     7  44
eg. paths_to_leaves t1 
    ==>
    [[0;1]; [1;0]]
 *)
let rec paths_to_leaves t =
  failwith "implement"

(*
eg. paths_to_nodes t1 
    ==>
    [[]; [0]; [0;1]; [1]; [1;0]]
 *)
let rec paths_to_nodes t =
  failwith "implement"

  (* function for tuple *)
(* fst;; -->  first value of the tuple *)
(* snd;; --> second  value of the tuple *)
(* moving from curried to uncurried *)








