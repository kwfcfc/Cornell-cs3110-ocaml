module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  val make : int -> int -> t
  (** [make n d] represents n/d, a fraction with numerator [n] and denominator
      [d]. Requires d <> 0. *)

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end

(* Exercise: fraction *)
module FractionInt : Fraction = struct
  type t = int * int

  let make n = function
    | 0 -> raise Division_by_zero
    | non_zero -> (n, non_zero)

  let numerator x = fst x
  let denominator x = snd x
  let to_string x = Printf.sprintf "%d / %d" (fst x) (snd x)
  let to_float x = (x |> fst |> float_of_int) /. (x |> snd |> float_of_int)
  let add (n1, d1) (n2, d2) = ((n1 * d2) - (n2 * d1), d1 * d2)
  let mul (n1, d1) (n2, d2) = (n1 * n2, d1 * d2)
end

(* Exercise: fraction reduced *)
module FractionReduced : Fraction = struct
  type t = int * int

  let rec gcd x y =
    if x = 0 then y else if x < y then gcd (y - x) x else gcd y (x - y)

  let reduce (n, d) =
    let g = gcd (abs n) (abs d) in
    let reduced_n, reduced_d = (n / g, d / g) in
    if reduced_n = 0 then (0, 1)
    else if reduced_d < 0 then (-reduced_n, -reduced_d)
    else (reduced_n, reduced_d)

  let make n d = if d = 0 then raise Division_by_zero else reduce (n, d)
  let numerator x = fst x
  let denominator x = snd x
  let to_string x = Printf.sprintf "%d / %d" (fst x) (snd x)
  let to_float x = (x |> fst |> float_of_int) /. (x |> snd |> float_of_int)
  let add (n1, d1) (n2, d2) = reduce ((n1 * d2) - (n2 * d1), d1 * d2)
  let mul (n1, d1) (n2, d2) = reduce (n1 * n2, d1 * d2)
end
