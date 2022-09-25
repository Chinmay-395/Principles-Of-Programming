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
                              [ Node (14 , [])]
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

let rec count_nodes (Node(_, sub)) =
    List.fold_left (fun n t -> n + count_nodes t) 1 sub;;




let rec height =
  function
  | Node (_, []) -> 1
  | Node (_, x::xs) -> 
    let sum = List.fold_left (+) 0 in
    2 + sum (max(List.map height xs) (List.map height [x]))

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


let rec height'' =
  function
  | Node (_, []) -> 1
  | Node (_, _::xs) -> 1 + List.fold_left max 0 (List.map height'' xs);;
