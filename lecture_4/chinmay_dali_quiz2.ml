(* Chinmay Dali *)
type 'a bt = Empty |  Node of 'a*'a bt*'a bt
(* Helper function that builds a leaf *)
let leaf n = Node(n,Empty,Empty)
(* Two sample binary trees.
   The first one is a BST, but not the second one *)
let ex1 = Node(12,
              Node(7,Empty,leaf 10),
               Node(24,
                    leaf 14,
                    Empty))
let ex2 = Node(12,
              leaf 7,
               Node(24,
                    leaf 30,
                    Empty))

(** returns smallest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)

let rec mint t =
  match t with
  | Node(d,Empty,Empty) -> d
  | Node(d,lt,Empty) -> min d (mint lt)
  | Node(d,Empty,rt) -> min d (mint rt)
  | Node(d,lt,rt) -> min (mint lt) (mint rt)
  | Empty -> failwith "should not be the case" (*failwith "should not be the case"*)
(** returns largest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec maxt t =
    match t with
  | Node(d,Empty,Empty) -> d
  | Node(d,lt,Empty) -> max d (maxt lt )
  | Node(d,Empty,rt) -> max d (maxt rt)
  | Node(d,lt,rt) -> max (maxt lt) (maxt rt)
  | Empty -> failwith "should not be the case" (*failwith "should not be the case"*)
(* Determines whether a binary is a binary search tree *)

(* let rec helper=
fun t low high ->
match t with
| Empty -> true
| Node(d,Empty,Empty) -> true
| Node(d,lt,Empty) -> 


let rec helper :int bt->int-> int->bool=
fun t low high ->
match t with
| Empty -> true
| Node(d,lt,rt) -> if ((d < low) && (d>high))
                    then (helper lt low d && helper rt d high)
                    else false                    
let rec is_bst t = helper t Int.min_int Int.max_int;; *)

(** adds v to the bst t. 
    Should fail with failwith if v is already in the tree.
    Otherwise, returns updated tree *)
let rec add v t =
  match t with
  | Empty -> leaf v
  | Node(d,Empty,Empty) when d=v -> failwith "Already exists ie duplicate key" 
  | Node(d,lt,rt) -> if(v<d) then Node(d, add v lt, rt) else Node(d, lt,add v rt)
           
(* Remove a value from a BST (Extra-credit)
   Should fail (with failwith) if the value is not in tree *)
let rec find_bst t k =
  match t with 
  | Empty -> false
  | Node(d,lt,rt) -> if (d=k) then true else find_bst lt k && find_bst rt k;;

  (*Check the inorder succesor and inoder predessor  *)
  let rec find_max t =
    match t with
    | Empty -> failwith "cannot happen"
    | Node(d,lt,Empty) -> d
    | Node(d,lt,rt) -> find_max rt;;
let rec rem v t =
  match t with 
  | Empty -> failwith "Not Found"
  | Node(d,Empty,Empty) when d=v -> Empty
  | Node(d,lt,Empty) when d=v -> lt
  | Node(d,Empty,rt) when d=v -> rt
  | Node(d,lt,rt) when d=v -> 
      let m = find_max lt in Node(m, rem m lt,rt )
  | Node(d,lt,rt) -> if(v<d) then Node(d, rem v lt, rt) else Node(d,lt, rem v rt);;

 