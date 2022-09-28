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


let rec foldr : ('a -> 'b -> 'b)  -> 'b -> 'a list -> 'b =
  fun f a l ->
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)
(* How to implement inorderTraversal using higher-order-function
   *)
let rec inorderTraversal t =
  match t with
  | Node(_,[]) -> n::[]
  | Node(k,subt) -> List.fold_left (fun acc mt -> acc::[] @ inorderTraversal mt) [] subt 
    (* List.fold_right (fun acc t -> k::acc @ preOrderTraversal t) [] sub;; *)


(* f(f(f(f(f(f(f(12 :: []))::33)::14)::37)::77)::48)::103 *)