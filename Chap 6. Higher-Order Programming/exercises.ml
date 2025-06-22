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

(* Exercise: library uncurried *)
let uncurried_append (lst0, lst1) = List.append lst0 lst1
let uncurried_compare (string0, string1) = Char.compare string0 string1
let uncurried_max (element1, elemtnt2) = Stdlib.max element1 elemtnt2

(* Exercise: Map composition *)
let map_composition f g (lst : 'a list) = List.map (fun x -> g x |> f) lst

(* Exercise: More list fun*)
let greater3 = List.filter (fun s -> String.length s > 3)
let add1f = List.map (fun x -> x +. 1.0)

let join lst sep =
  match lst with
  | [] -> ""
  | [ s ] -> s
  | h :: t -> List.fold_left (fun acc elt -> acc ^ sep ^ elt) h t

(* Exercise: association list keys *)
let keys lst = lst |> List.split |> fst |> List.sort_uniq compare

(* Exercise: valid matrix *)
let is_valid_matrix (lst : int list list) =
  match lst with
  | [] -> false
  | head_row :: t ->
    let row_length = List.length head_row in
    row_length > 0 && List.for_all (fun row -> List.length row = row_length) t

(* Exercise: row vector add *)
let add_row_vectors = List.map2 ( + )

(* Exercise: matrix add *)
let add_matrices = List.map2 add_row_vectors

(* Exercise: matrix multiply *)
let vector_dot_product =
  List.fold_left2 (fun acc left right -> acc + (left * right)) 0

let get_column n = List.map (fun row -> List.nth row n)

let matrix_transpose matrix =
  let row1 = List.nth matrix 0 in
  List.mapi (fun i _row -> List.map (fun row -> List.nth row i) matrix) row1

let multiply_matrices left right =
  let rightT = matrix_transpose right in
  List.map
    (fun left_row ->
       List.map
         (fun right_column -> vector_dot_product left_row right_column)
         rightT)
    left
