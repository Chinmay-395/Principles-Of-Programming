type program = int list
val square : program
val letter_e : program
val mirror_func : int -> int
val mirror_image : int list -> int list
val rotate_90_func : int -> int
val rotate_90_letter : int list -> int list
val rotate_90_word : int list list -> int list list
val repeat : int -> 'a -> 'a list
val flatten : 'a list list -> 'a list
val pantograph : int -> int list -> int list
val pantograph_nm : int -> int list -> int list
val foldr : ('a -> 'b -> 'c -> 'c) -> 'c -> 'b -> 'a list -> 'c
val pantograph_f : int -> int list -> int list
val directions : int * int -> int -> int * int
val coverage : int * int -> int list -> (int * int) list
val compress : int list -> (int * int) list
val uncompress : (int * int) list -> int list
val uncompress_m : ('a * int) list -> 'a list
val uncompress_f : 'a list -> 'a list
val helperFuncOptimize : int list -> int list
val optimize : int list -> int list
