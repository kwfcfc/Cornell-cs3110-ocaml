(* Exercise: complex synonym *)
module type ComplexSig = sig
  type t = float * float

  val add : t -> t -> t
  val zero : t
end

(* Exercise: complex encapsulation *)
module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(*
   - remove zero from the structure: The value zero is required but not
     provided. Reason: the value zero is required in the signature that
     specifies the functions and variables needed in the module.

   - remove add from the signature: No error, but compiler throws an error for
     unused value add in the struct, and the client cannot use add function.

   - change zero in the structure to let zero = 0, 0: The value zero does not
     match because it requires float * float but int is not compatible with
     float.
*)

(* Exercise: big list queue *)

(** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let open Queue in
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty
