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

let rec auxi_func n acc = if n = 0 then acc else auxi_func (n-1) (n::acc)

(* 
(* let rec paths_to_leaves t =
  let rec helper acc depth = 
  match depth,t with
  | depth,Node(n,[]) -> auxi_func depth []
  | depth,Node(n,subl) -> List.map helper acc subl in helper [] (List.length subl)
  
  *) *)

let rec mapHelper f acc l =
  match l with
  | [] -> []
  | h::[] -> f acc h
  | h::t -> f acc h @ mapHelper f acc t

(* paths_to_leaves 
initally the accumulator will be 0, the accumulator is the count of depth
When we get to a leaf we `acc` 
when you get to next child from the accumulator should be incremented by 1
*)

let rec paths_to_leaves acc t =
  match t with
  | Node(k, []) -> []
  | Node(k,h::t) -> [[acc]] @ paths_to_leaves 0 h @ mapHelper paths_to_leaves (acc+1) t

