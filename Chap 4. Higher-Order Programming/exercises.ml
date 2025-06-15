(* Exercise: twice, no arguments *)
let double x = 2 * x
let square x = x * x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

(* quad is a function that is contructed from a higher-order function twice that
   takes in an int -> int function and a int.
*)

(* Exercise: mystery operator 1 *)
let ( $ ) f x = f x

(* This operator takes the left operand as a function and applies it to the
   right operand. So square $ 2 + 2 will give 16 because it is (sqaure 4), and
   square 2 + 2 will give 6 because (square 2) + 2 = 4 + 2 = 6. This implies the
   ($) is executed after (+) addition, and (+) is after function square.
*)

(* Exercise: mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f

(* This operator takes two functions and return a new function that chains the
   output of right operand as the input to the left operand, then returns the
   output from left operand function.
*)

(* Exercise: repeat *)
let rec repeat f n x =
  match n with
  | 0 -> x
  | t -> repeat f (t - 1) (f x)

(* Exercise: product *)
let product_left = List.fold_left ( *. ) 1.
let product_right = List.fold_right ~f:( *. ) ~init:1.

(* Exercise: terse product, already one line of code *)

(* Exercise: sum_cube_odd *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

let sum_cube_odd_brackets n =
  List.fold_left ( + ) 0
    (List.map
       (fun x -> x * x * x)
       (List.filter (fun x -> x mod 2 <> 0) (0 -- n)))

(* Exercise: sum_cube_odd in pipeline *)
let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x mod 2 <> 0)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

(* Exercise: exists *)
let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t

let exists_fold p = List.fold_left (fun acc elt -> acc || p elt) false
let exists_lib = List.exists

(* Exercise: Account Balance *)
let balance_left = List.fold_left (fun acc elt -> acc -. elt)
let balance_right acc lst = acc -. List.fold_right ( +. ) lst 0.

let rec balance_rec acc = function
  | [] -> acc
  | h :: t -> balance_rec (acc -. h) t
