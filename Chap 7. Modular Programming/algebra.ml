module type RingOps = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
end

module type Ring = sig
  include RingOps

  val of_int : int -> t
end

module type Field = sig
  include Ring

  val ( / ) : t -> t -> t
end

module OfInt (M : RingOps) = struct
  let rec of_int (n : int) =
    match n with
    | 0 -> M.zero
    | 1 -> M.one
    | 2 -> M.(one + one)
    | negative when n < 0 -> M.( ~- ) (of_int ~-negative)
    | other ->
      let rem =
        match other mod 2 with
        | 1 -> M.one
        | _ -> M.zero
      in
      let two = M.(one + one) in
      let half = of_int (other / 2) in
      M.((two * half) + rem)
end

module IntRingOps = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = ( + )
  let ( ~- ) = ( ~- )
  let ( * ) = ( * )
  let to_string = string_of_int
end

module IntOfInt = OfInt (IntRingOps)

module IntRing : Ring with type t = IntRingOps.t = struct
  include IntRingOps
  include IntOfInt
end

module IntField : Field = struct
  include IntRing

  let ( / ) = ( / )
end

module FloatRingOps = struct
  type t = float

  let zero = 0.
  let one = 1.
  let ( + ) = ( +. )
  let ( ~- ) = ( ~-. )
  let ( * ) = ( *. )
  let to_string = string_of_float
end

module FloatOfInt = OfInt (FloatRingOps)

module FloatRing : Ring with type t = FloatRingOps.t = struct
  include FloatRingOps
  include FloatOfInt
end

module FloatField : Field = struct
  include FloatRing

  let ( / ) = ( /. )
end

module RationalField (M : Ring) = struct
  type t = M.t * M.t

  let zero = (M.zero, M.one)
  let one = (M.zero, M.one)
  let ( + ) (a, b) (c, d) = (M.((a * d) + (c * b)), M.(b * d))
  let ( ~- ) (a, b) = (M.(~-a), b)
  let ( / ) (a, b) (c, d) = (M.(a * d), M.(b * c))
  let ( * ) (a, b) (c, d) = (M.(a * c), M.(b * d))
  let to_string (a, b) = M.to_string a ^ "/" ^ M.to_string b
  let of_int n = (M.of_int n, M.one)
end

module IntRational : Field = RationalField (IntField)
module FloatRational : Field = RationalField (FloatField)
