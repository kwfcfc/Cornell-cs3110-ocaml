(* Exercise: list expressions *)
let bracket_list1 = [ 1; 2; 3; 4; 5 ]
let bracket_list2 = [ 1; 2; 3; 4; 5 ]
let bracket_list3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Exercise: product *)
let rec product = function
  | [] -> 1
  | h :: t -> h * product t

(* Exercise: concat*)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

(* Exercise: patterns *)
let rec match_bigred = function
  | [] -> false
  | "bigred" :: _ -> true
  | _ :: t -> match_bigred t

let match_2or4 = function
  | [ _; _ ] | [ _; _; _; _ ] -> true
  | _ -> false

let match_first2 = function
  | first :: second :: _ -> first = second
  | _ -> false

(* Exercise: library *)
let fifth (lst : int list) =
  if List.length lst >= 5 then List.nth lst (5 - 1) else 0

let mysort (lst : int list) = List.rev (List.sort Stdlib.compare lst)

(* Exercise: library puzzle *)
let last_in_list lst = List.hd (List.rev lst)
let any_zeros (lst : int list) = List.exists (fun any -> any = 0) lst

(* Exercise: take drop*)

let rec take n lst =
  match (n, lst) with
  | 0, _ -> []
  | _, [] -> []
  | k, h :: t -> h :: take (k - 1) t

(* drop function, already tail recursive *)
let rec drop n lst =
  match (n, lst) with
  | 0, t -> t
  | _, [] -> []
  | k, _ :: t -> (drop [@tailcall]) (k - 1) t

let take_tail n lst =
  let rec helper acc n lst =
    match (n, lst) with
    | 0, _ | _, [] -> acc
    | k, h :: t -> (helper [@tailcall]) (h :: acc) (k - 1) t
  in
  List.rev (helper [] n lst)

(* Exercise: unimodal *)

let is_unimodal (lst : int list) =
  (*
     * pass_list get the head from its previous call, determine if
  *)
  let rec pass_list (h : int) (increasing : bool) (lst : int list) =
    match lst with
    | [] -> true
    | k :: t -> (
      match (compare h k, increasing) with
      | 0, _ | -1, true | 1, false -> pass_list k increasing t
      | -1, false -> false
      | 1, true -> pass_list k (not increasing) t
      | _, _ -> assert false (* compare should not return any other integer*))
  in
  match lst with
  | [] -> true
  | h :: t -> pass_list h true t

(* Exercise: powerset *)
let rec powerset (lst : int list) =
  let rec map_helper (n : int) (set : int list list) =
    match set with
    | [] -> [ [ n ] ]
    | h :: t -> (n :: h) :: h :: map_helper n t
  in
  match lst with
  | [] -> []
  | h :: t -> map_helper h (powerset t)

(* Exercise: Print int list rec*)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
    Printf.printf "%i\n" h;
    print_int_list t

(* Exercise: print int list iter*)
let print_int_list' lst = List.iter (fun x -> Printf.printf "%i\n" x) lst

(* Exercise: student *)
type student = { first_name : string; last_name : string; gpa : float }

let my_student = { first_name = "Goba"; last_name = "Lewis"; gpa = 3.89 }

let get_name = function
  | { first_name; last_name; _ } -> (first_name, last_name)

let create_student first_name last_name gpa = { first_name; last_name; gpa }

(* Exercise: pokerecord *)
type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }

(* Exercise: Safe hd and tl*)
let safe_hd = function
  | [] -> None
  | x :: _ -> Some x

let safe_tl = function
  | [] -> None
  | _ :: t -> Some t

(* Exercise: pokefun *)
let max_hp (pokemon_list : pokemon list) =
  let rec max_helper max = function
    | None -> max
    | Some curr_list -> (
      match safe_hd curr_list with
      | None -> max
      | Some poke ->
        if poke.hp > max.hp then
          (max_helper [@tailcall]) poke (safe_tl curr_list)
        else (max_helper [@tailcall]) max (safe_tl curr_list))
  in
  match safe_hd pokemon_list with
  | Some poke -> Some (max_helper poke (safe_tl pokemon_list))
  | None -> None

(* Exercise: date before*)
type date = int * int * int

let is_before date1 date2 =
  let (y1, m1, d1), (y2, m2, d2) = (date1, date2) in
  match compare y1 y2 with
  | -1 -> true
  | 1 -> false
  | _ -> (
    match compare m1 m2 with
    | -1 -> true
    | 1 -> false
    | _ -> d1 < d2)

(* Exercise: earliest date *)
let earliest (dates : date list) =
  let rec min_helper earliest = function
    | None -> earliest
    | Some dates_list -> (
      match safe_hd dates_list with
      | None -> earliest
      | Some date ->
        if is_before date earliest then
          (min_helper [@tailcall]) date (safe_tl dates_list)
        else (min_helper [@tailcall]) earliest (safe_tl dates_list))
  in
  match safe_hd dates with
  | Some date -> Some (min_helper date (safe_tl dates))
  | None -> None

(* Exercise: assoc list *)

(* functions from section 8. *)

(** [insert k v lst] is an association list that binds key [k] to value [v] and
    otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to value
    [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let simple_map = [] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three"
let two_in_simple_map = lookup 2 simple_map
let four_in_simple_map = lookup 4 simple_map

(* Exercise: cards *)
type suit = Spade | Heart | Diamond | Club

type rank =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type card = suit * rank

let card1 = (Club, Ace)
let card2 = (Heart, Queen)
let card3 = (Diamond, Two)
let card4 = (Spade, Seven)

(* Exercise: matching *)
let match1 = [ None ]
let match2 = [ Some 8964; None ]
let match3 = [ None ]
let match4 = [ 8964 ]
let match5 = []

(* Exercise: quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign = if x > 0 then Pos else if x = 0 then Zero else Neg

let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None

let quadrant_when : int * int -> quad option = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None

(* Exercise: depth*)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec depth : 'a tree -> int = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

(* Exercise: shape *)
let rec same_shape a b =
  match (a, b) with
  | Leaf, Leaf -> true
  | Leaf, _ | _, Leaf -> false
  | Node (_, left1, right1), Node (_, left2, right2) ->
    same_shape left1 left2 && same_shape right1 right2

(* Exercise: list max exn *)
let list_max lst =
  let rec max_helper max = function
    | [] -> max
    | head :: curr_list ->
      if head > max then (max_helper [@tailcall]) head curr_list
      else (max_helper [@tailcall]) max curr_list
  in
  match lst with
  | [] -> raise (Failure "list_max")
  | h :: t -> max_helper h t

(* Exercise: list max exn string *)
let list_max_string = function
  | [] -> "empty"
  | t -> string_of_int (list_max t)

(* Exercise: is_bst *)
let is_bst (root : ('a * 'b) tree) =
  let rec valid_bst_helper some_min some_max = function
    | Leaf -> true
    (* I assume ('a * 'b) is a key-value pair here. *)
    | Node ((key, _), left, right) ->
      let within_bound =
        (match some_min with
          | Some min_val -> min_val <= key
          | None -> true)
        &&
        match some_max with
        | Some max_val -> max_val >= key
        | None -> true
      in
      within_bound
      && valid_bst_helper some_min (Some key) left
      && valid_bst_helper (Some key) some_max right
  in
  valid_bst_helper None None root

(* Exercise: quadrant poly *)
let sign_poly = function
  | x when x < 0 -> `Neg
  | x when x = 0 -> `Zero
  | _ -> `Pos

let quadrant_poly x y =
  match (sign_poly x, sign_poly y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None
