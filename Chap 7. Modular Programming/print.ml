(* Exercise: ToString *)
module type ToString = sig
  type t

  val to_string : t -> string
end

(* Exercise: Print *)
module Print (M : ToString) = struct
  let print (value : M.t) = M.to_string value
end

(* Exercise: Print Int *)
module PrintInt = Print (Int)

(* Exercise: Print String *)
module MyString = struct
  type t = string

  let to_string s = s
end

module PrintString = Print (MyString)

(* Exercise: Print Reuse *)

(* The Print Functor requires the to_string function and a type for the module
   to enable better reuse so that any module that implements this method can
   print. *)

(* Exercise: Print String revisited*)

module StringWithPrint = struct
  include String
  include PrintString
end
