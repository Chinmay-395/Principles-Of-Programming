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

let rec path_helper acc lst = 
  match lst with
  |[] -> []
  |Node(n, lst1) :: y -> path_helper 0 lst1 @ path_helper (acc+1) y

let rec paths_to_leaves t =
  match t with
  | Node(n,[]) -> [n]
  | Node(n,lst) -> path_helper 0 lst