(* HW2: Assignment 2 
  Name: Chinmay Dali   
*)
type 'a gt = Node of 'a *( ' a gt ) list

let t : int gt =
Node (33 ,
        [ Node (12 ,[]) ;
          Node (77 ,
                    [ Node (37 ,
                              [ Node (14 , [])]
                            ) ;
                      Node (48 , []) ;
                      Node (103 , [])
                    ]       
              )
        ]
    )

let t2 : int gt =
Node (33 ,
        [ Node (12 ,[
                      Node(1,[
                              Node(2,[
                                Node(3,[])
                              ]);
                              Node(4,[]);
                              Node(5,[]);
                            ])
                    ]) ;
          Node (77 ,
                    [ Node (37 ,
                              [ Node (14 , [])]
                            ) ;
                      Node (48 , []) ;
                      Node (103 , [])
                    ]       
              )
        ]
    )


let t' : int gt =
Node (33 ,
        [ Node (12 ,[]) ;
          Node (77 ,
                    [ Node (37 ,
                              [ Node (14 , [Node(15,[])])]
                            ) ;
                      Node (48 , []) ;
                      Node (103 ,[
                                Node(90,[]);
                                Node(91,[]);
                                Node(92,[])
                            ])
                    ]       
              )
        ]
    )


let mk_leaf : 'a -> 'a gt =
fun n ->Node (n ,[])

(* Moreover, in this
assignment general trees will be assumed to be non-empty. *)


(* Exercise-1 *)
let rec height =
  function
  | Node (_, []) -> 1
  | Node (_, xs) -> 1 + List.fold_left max 0 (List.map height xs);;

(* Exercise-2 *)
let rec size (Node(k, sub)) =
    List.fold_left (fun n t -> n + size t) 1 sub;; (** t is the input*)


let rec maper f l =
   match l with
  | [] ->  failwith "Tree cannot be empty" 
  | h :: [] -> f h 
  | h::t -> f h @ maper f t;;

let rec preOrderTraversal t =
  match t with
  | Node(n,[]) -> n::[]
  | Node(n,subt) -> n :: (maper preOrderTraversal subt);;


  (* Implementing preOrderTraversal using higher-order-function *)
let rec preOrderTraversal2 (Node(d,listOfChild)) = 
  d :: List.flatten (List.map preOrderTraversal2 listOfChild) 


let rec inorderTraversal t = 
  match t with
  | Node(k,[]) -> [[k]]
  | Node(k,h::t) -> (inorderTraversal h) @ [[k]] @ List.flatten(List.map inorderTraversal t);;

(* let rec mirror t = 
  match t with
  | Node(k,[]) -> k::[]
  | Node(k,h::t) -> List.flatten(List.map mirror t) @ [k] @ (mirror h);; *)


let rec postOrderTraversal t =
  match t with
  (* | Node(n,[]) -> n::[] *)
  | Node(k,subt) -> List.fold_left (fun acc mt -> postOrderTraversal mt @ acc) [] subt @ [k]  ;;


let rec inorderTraversal t =
  match t with
  (* | Node(n,[]) -> n::[] *)
  | Node(k,subt) -> List.fold_left (fun acc mt -> inorderTraversal mt @ acc) [] subt @ [k]  
    (* List.fold_right (fun acc t -> k::acc @ preOrderTraversal t) [] sub;; *)




(* paths_to_leaves 
initally the accumulator will be 0, the accumulator is the count of depth
When we get to a leaf we `acc` 
when you get to next child of the same node then, the accumulator should be incremented by 1
*)
  (* Attempt-1 *)
  (* let rec path_helper acc lst = 
  match lst with
  |[] -> []
  |Node(n, lst1):: y -> [acc] @ (path_helper 0 lst1) @ ([acc] @ (path_helper (acc+1) y))

let rec paths_to_leaves1 t =
  match t with
  | Node(n,[]) -> [n]
  | Node(n,lst) -> path_helper 0 lst *)

  (* Attempt-2 *)
(* let rec mapHelper2 f acc l =
  match l with
  | [] -> failwith "A tree cannot be empty"
  | h::[] -> f acc h
  | h::t -> f acc h @ mapHelper2 f acc t

let rec paths_to_leaves_helper2 acc t =
  match t with
  | Node(k, []) -> [[acc]]
  | Node(k,h::t) -> mapHelper2 paths_to_leaves_helper2 (acc+1) t

let paths_to_leaves2 t = paths_to_leaves_helper2 0 t;; *)
(* mapi *)
(* Attempt-2.1 *)
(* let rec mapHelper f l =
  match l with
  | [] -> failwith "A tree is never empty"
  | h::[] -> f h
  | h::t -> f h @ mapHelper f t


let rec append_to_front n l = 
    match l with
    |[] -> []
    |h :: t -> (n :: h) :: append_to_front n t;;



let rec paths_to_leaves t = 
    match t with
    |Node(d,[]) -> [[d]] (** Here handle empty list case *)
    |Node(d, l) -> append_to_front d (mapHelper paths_to_leaves l);;
 *)


(* Attempt 3 *)

(* The trace output of the size
size t;;
size <--
  Node (33,
   [Node (12, []);
    Node (77,
     [Node (37, [Node (14, [])]); 
     Node (48, []); Node (103, [])])])
size <-- Node (12, [])
size --> 1
size <--
  Node (77,
   [Node (37, [Node (14, [])]); Node (48, []); Node (103, [])])
size <-- Node (37, [Node (14, [])])
size <-- Node (14, [])
size --> 1
size --> 2
size <-- Node (48, [])
size --> 1
size <-- Node (103, [])
size --> 1
size --> 5
size --> 7
             *)
let rec maper f l =
   match l with
  | [] ->  failwith "Tree cannot be empty" 
  | h::[] -> f h 
  | h::t -> f h @ maper f t



(* let rec foldingSomething (Node(k, sub)) =
    List.fold_left (fun n lst -> n :: maper foldingSomething lst) [] sub;;  *)

let rec map : ( 'a -> 'b ) -> 'a list -> 'b list  =
  fun f l ->
   match l with
  | [] -> []
  | h::t -> f h :: map f t

let rec foldt f t = 
    match t with
    |Node(d, l) -> f d (map (foldt f) l);;
(* Attempt-4 *)
(* let rec paths_to_leaves4 t =
  match t with
  | Node(k,[]) -> [[]]
  | Node(k,ch) -> map (fun x y -> x::y::[]) ch;; *)

(* Attempt-5 *)

(* let rec auxi_func n acc = if n = 0 then acc else auxi_func (n-1) (n::acc)

let rec path_to_leaves4 depth_array list =
  let rec prepend index depth_array (Node(x,h::t)) =
    if(h == []) then depth_array else prepend (index+1) (index::depth_array) (Node(x,ch)) in
      let rec aux acc = function
        | [] -> acc
        | h::t

         *)

(* Attempt-6 *)

let rec maper f l =
   match l with
  | [] ->  failwith "Tree cannot be empty" 
  | h :: [] -> f h 
  | h::t -> f h @ maper f t;;

let rec pathToLeaves t =
  match t with
  | Node(n,[]) -> [n]::[]
  | Node(n,subt) -> List.mapi (fun index _ -> index::[])(maper pathToLeaves subt);;