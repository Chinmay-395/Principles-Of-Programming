open Ast
open ReM
open Dst


let rec type_of_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    type_of_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    type_of_expr e1 >>= fun t1 ->
    type_of_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    type_of_expr e1 >>= fun t1 ->
    type_of_expr e2 >>= fun t2 ->
    type_of_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    type_of_expr e >>= fun t ->
    extend_tenv id t >>+
    type_of_expr body
  | Proc(var,t1,e) ->
    extend_tenv var t1 >>+
    type_of_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | App(e1,e2) ->
    type_of_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    type_of_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec(id,param,tParam,tRes,body,target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     type_of_expr body >>= fun t ->
     if t=tRes 
     then type_of_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")

  (* references *)
  | BeginEnd(es) ->
    List.fold_left (fun r e -> r >>= fun _ -> type_of_expr e) (return UnitType) es 
  | NewRef(e) ->
    error "type_of_expr: implement"  
  | DeRef(e) ->
    error "type_of_expr: implement"  
  | SetRef(e1,e2) ->
    error "type_of_expr: implement"  

  (* pairs *)
  | Pair(e1,e2) ->
    error "type_of_expr: implement"
  | Unpair(id1,id2,e1,e2) ->
    error "type_of_expr: implement"
      
  (* lists *)
  | EmptyList(t) ->
    error "type_of_expr: implement"  
  | Cons(h, t) ->
    error "type_of_expr: implement"  
  | Null(e) ->
     error "type_of_expr: implement"  
  | Hd(e) ->
    error "type_of_expr: implement"  
  | Tl(e) ->
    error "type_of_expr: implement"  

  (* trees *)
  | EmptyTree(t) ->
    error "type_of_expr: implement"  
  | Node(de, le, re) ->
    error "type_of_expr: implement"  
  | NullT(t) ->
    error "type_of_expr: implement"  
  | GetData(t) ->
    error "type_of_expr: implement"  
  | GetLST(t) ->
    error "type_of_expr: implement"  
  | GetRST(t) ->
    error "type_of_expr: implement"  


  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug called!"
  | _ -> error "type_of_expr: implement"    



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> type_of_expr
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> type_of_expr
  in run_teac (c >>= fun t -> return @@ Ast.string_of_texpr t)

