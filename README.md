# Principles-Of-Programming


```ocaml

let rec height =
  function
  | Node (_, []) -> 1
  | Node (_, x::xs) -> 
    let sum = List.fold_left (+) 0 in
    1 + sum (max(List.map height xs) (List.map height [x]))

let rec height_ =
  function
  | Node (_, []) -> 1
  | Node (_, _::xs) -> 
    let sum = List.fold_left (+) 0 in
    2 + sum (List.map height xs);;

let rec height'' =
  function
  | Node (_, []) -> 1
  | Node (_, _::xs) -> 1 + List.fold_left max 0 (List.map height'' xs);;


let rec foldl f a l =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t


(* let rec inorder =
function
| Node(x,[]) -> [x]
| Node(x, lst) -> (List.fold_left inorder lst) :: [x] *)

(* let rec tree_map f (Node (v, sub)) =
  Node(f v, List.map (tree_map f) sub) *)

(* Code that executes


let rec foldr  =
  fun f a n l ->
  match l with
  | [] -> a
  | h::t -> f h n (foldr f a n t);;
 
let rec count_nodes (Node(k, sub)) =
    List.fold_left (fun n t -> n + count_nodes t) 1 sub;;
(* The fold_left performs an preOrder Traversal *)

let rec maper f l = function
  | [] ->  failwith "qwewqe" 
  | h :: [] -> f h 
  | h::t -> if(List.length t > 0) then maper f t else f h;;

let rec preOrderTraversal t =
  match t with
  | Node(n,[]) -> n::[]
  | Node(n,subt) -> n :: (maper preOrderTraversal subt);;   

let rec map'' f l =
   match l with
  | [] ->  failwith "Impossible" 
  | h :: [] -> f h 
  | h::t -> f h @ map'' f t;;
*)

```

```
<!-- Backup at 5pm Sep 29 2022 (* HW2: Assignment 2 
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
fun n ->
Node (n ,[])

(* Moreover, in this
assignment general trees will be assumed to be non-empty. *)

let rec size (Node(k, sub)) =
    List.fold_left (fun n t -> n + size t) 1 sub;; (** t is the input*)


let rec height =
  function
  | Node (_, []) -> 1
  | Node (_, xs) -> 1 + List.fold_left max 0 (List.map height xs);;



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
  | Node(k,[]) -> k::[]
  | Node(k,h::t) -> (inorderTraversal h) @ [k] @ List.flatten(List.map inorderTraversal t);;
(* f(f(f(f(f(f(f(12 :: []))::33)::14)::37)::77)::48)::103 *)


let rec postOrderTraversal t =
  match t with
  (* | Node(n,[]) -> n::[] *)
  | Node(k,subt) -> List.fold_left (fun acc mt -> postOrderTraversal mt @ acc) [] subt @ [k]  ;;


let rec inorderTraversal t =
  match t with
  (* | Node(n,[]) -> n::[] *)
  | Node(k,subt) -> List.fold_left (fun acc mt -> inorderTraversal mt @ acc) [] subt @ [k]  
    (* List.fold_right (fun acc t -> k::acc @ preOrderTraversal t) [] sub;; *)



let rec path_helper acc lst = 
  match lst with
  |[] -> []
  |Node(n, lst1):: y -> [acc] @ (path_helper 0 lst1) @ (path_helper (acc+1) y)

let rec paths_to_leaves t =
  match t with
  | Node(n,[]) -> [n]
  | Node(n,lst) -> path_helper 0 lst

let rec auxi_func n acc = if n = 0 then acc else auxi_func (n-1) (n::acc)



    
 -->
```