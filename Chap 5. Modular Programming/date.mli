(* If only the abstract type is given, dune utop will complain about unused 
   variables such as make_date because they are not declared in the interface*)
(* type date *)

type date = { month : int; day : int }

val make_date : int -> int -> date
val get_month : date -> int
val get_day : date -> int
val to_string : date -> string
val format : Format.formatter -> date -> unit
