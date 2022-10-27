arrays are mutuable but list are immutable; if update a list, then a new list with new values come in
but it will not update the origin list. In array it does update the list.

ref is exactly like malloc

!y --> is like *y in C language

ref -> malloc
! -> de-allocation or getting the value
:= -> assignment

referencial transparency

begin & end functionaily

(y:= !y+1; !y) + !y;; --> constructors work from right to left thus giving the o/p as 1 rather than 2 (by intuition)

mutable is keyword


```ocaml
let add_last e ll =
    let rec helper (n: 'a node option) =function
        | None -> failwith "add last not possible"
        | Some node -> if node.next = None then node.next <- Some{data=e; next=None}
                else helper node.next 
    in
    if ll.size = 0
    then ll.head <- Some{data=e; next=None} 
    else helper ll.head
    ;
    ll.size <- ll.size+1;
```

```ocaml
let mapll f ll : ('a -> 'b) -> 'a ll -> unit =
    let rec helper n =
    match n with
    | None -> ()
    | Some node ->
            node.data <- f (node.data); 
        helper(node.next)
    in helper ll.head
```