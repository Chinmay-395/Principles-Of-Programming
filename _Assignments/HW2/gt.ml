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

let t3 : int gt =
Node (33 ,
        [ Node (12 ,[]) ;
          Node (14 ,[]) ;
          Node (15 ,[]) ;
          Node (16 ,[]) ;
        ]
    )


let mk_leaf : 'a -> 'a gt =
fun n ->Node (n ,[])


(* Exercise-1 *)
let rec height =
  function
  | Node (_, []) -> 1
  | Node (_, xs) -> 1 + List.fold_left max 0 (List.map height xs);;

(* Exercise-2 *)
let rec size (Node(k, sub)) =
    List.fold_left (fun n t -> n + size t) 1 sub;; 





let rec postorder t =
  match t with
  (* | Node(n,[]) -> n::[] *)
  | Node(k,subt) -> List.fold_left (fun acc mt -> postorder mt @ acc) [] subt @ [k]  ;;


let rec inorderTraversal t =
  match t with
  (* | Node(n,[]) -> n::[] *)
  | Node(k,subt) -> List.fold_left (fun acc mt -> inorderTraversal mt @ acc) [] subt @ [k]  
    (* List.fold_right (fun acc t -> k::acc @ preOrderTraversal t) [] sub;; *)



(* Exercise-3 *)
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
    |Node(d,[]) -> [[d]] 
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
(* Using List.mapi functionality *)
(* let rec maper f l =
   match l with
  | [] ->  failwith "Tree cannot be empty" 
  | h :: [] -> f h 
  | h::t -> f h @ maper f t;;

let rec pathToLeaves t =
  match t with
  | Node(n,[]) -> [n]::[]
  | Node(n,subt) -> List.mapi (fun index _ -> index::[])(maper pathToLeaves subt);; *)

(* Attempt-7 *)

(* update the tree in such a way that, 
root is -1 (node 33)
its first child will be 0 ie (node 12 is 0th)
its second child will be 1 ie (node 77 is 1st)
node 37 being 1st node of 77 therefore it will be 0

kind of

Node(-1,[ --> 33
  Node(0,[]), --> 12
  Node(1,[ --> 77
     Node(0,[ --> 37
       Node(0,[])--> 14
        ]);
       Node(1,[]); --> 48
       Node(2,[]); --> 103
      ])
  ])
])
*)

let rec map : ( 'a -> 'b ) -> 'a list -> 'b list  =
  fun f l ->
   match l with
  | [] -> []
  | h::t -> f h :: map f t
let rec maper f l =
   match l with
  | [] ->  failwith "Tree cannot be empty; Something went wrong" 
  | h :: [] -> f h 
  | h::t -> f h @ maper f t;;

let rec prepend n l = 
    match l with
    |[] -> []
    |h :: t -> (n :: h) :: prepend n t;;

let rec paths_to_leaves_helper t = 
    match t with
    |Node(n,[]) -> [[n]] 
    |Node(n, subt) -> prepend n (maper paths_to_leaves_helper subt);;


let rec convert_into_index l i = 
    match l with 
    |[] -> []
    |Node(n, subt)::t -> Node(i,subt) :: convert_into_index t (i+1);;

let rec update_tree t = 
    match t with
    |Node(n, subt) -> Node(n, map update_tree(convert_into_index subt 0));;

let remove_inital_root l = 
    match l with 
    |[] -> []
    |h :: t -> t;;

let paths_to_leaves t = map remove_inital_root (paths_to_leaves_helper (update_tree t));;

(* exercise 4 *)
let rec path_depth l = map List.length l;;

let rec is_leaf_perfect_helper l = 
    match l with
    |[] -> true
    |h::[]-> true
    |h::next::t -> if(h = next) then is_leaf_perfect_helper (next::t) else false;;

let is_leaf_perfect t = is_leaf_perfect_helper(path_depth(paths_to_leaves t));;

(* Exercise-5 *)
(* Implementing preOrderTraversal using higher-order-function *)
let rec preorder (Node(d,listOfChild)) = 
  d :: List.flatten (List.map preorder listOfChild) 

(* exercise 6 *)
let rec mirror t =
    match t with
    |Node(n, []) -> Node(n, [])
    |Node(n, subt) -> Node(n, List.rev(map mirror subt));;

(* exercise-7 *)
let rec mapt f t = 
    match t with 
    |Node(n, []) -> Node(f n, []);
    |Node(n, subt) -> Node(f n, map (mapt f) subt);;

(* exercise-8 *)
let rec foldt f t = 
  match t with
  |Node(n, subt) -> f n (map (foldt f) subt);;

let  sumt t = foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t;;
let  memt t e =foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t;;

(* exercise 9 *)
let mirror' t = foldt (fun i rs -> Node(i, List.rev(rs))) t;;


let paths_to_leaves_with_fold (t:'a gt) : int list list = 
  let rec aux (acc: int list) (t: 'a gt): int list list =
    match t with
    | Node(_,[]) -> [acc]
    | Node(k,ts) -> List.flatten (List.mapi(fun idx t -> aux (acc @ [idx]) t) ts) in aux [] t