(* HW2: Assignment 2 
  Name: Chinmay Dali   
*)
type 'a gt = Node of 'a *( ' a gt ) list

let t : int gt =
Node (33 ,
[ Node (12 ,[]) ;
Node (77 ,
[ Node (37 ,
[ Node (14 , [])]) ;
Node (48 , []) ;
Node (103 , [])])
])

let mk_leaf : 'a -> 'a gt =
fun n ->
Node (n ,[])