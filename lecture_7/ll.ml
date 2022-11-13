type 'a node = {
  mutable data:'a;
  mutable next: 'a node option}

type 'a ll = {
  mutable head: 'a node option;
  mutable size: int}

let l2 : int ll = {
  head = Some { data=1;
                next = Some {data=1; 
                            next =None}};
  size = 2
}


let add_first ll e =
  ll.head <- Some {data=e; next=ll.head};
  ll.size <- ll.size + 1;

  let list_of_llist ll =
    let rec helper n =
      match n with
      | None -> []
      | Some node -> node.data :: helper(node.next)
    in helper ll.head

let add_last e ll:unit