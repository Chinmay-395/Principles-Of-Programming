(* A mini Logo Program *)
(* This requires a list of tuples to emulate dictionary 
  [(0,"Pen Down");(1,"Pen Up"); (2,"Move North"); (3,"Move East");(4,"Move South"); (5,"Move West")]   

Or use two lists

[0;1;2;3;4;5]
[Pen Down;Pen Up;Move North;Move East;Move South;Move West]  


*)







let findDirection n = 
  match n with
  | 0 -> "Pen Down"
  | 1 ->"Pen Up"
  | 2 ->"Move North"
  | 3 ->"Move East"
  | 4 ->"Move South"
  | 5 ->"Move West"
  | _ -> failwith "Incorrect Direction"

  