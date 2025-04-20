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
        if poke.hp > max.hp then max_helper poke (safe_tl curr_list)
        else max_helper max (safe_tl curr_list))
  in
  match safe_hd pokemon_list with
  | Some poke -> Some (max_helper poke (safe_tl pokemon_list))
  | None -> None

(* Exercise: date before*)
