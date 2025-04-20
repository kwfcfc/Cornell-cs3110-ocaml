(* Exercise: list expressions *)
let bracket_list1 = [ 1; 2; 3; 4; 5 ]
let bracket_list2 = [ 1; 2; 3; 4; 5 ]
let bracket_list3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Exercise: product *)
let rec product lst =
  match lst with
  | [] -> 1
  | h :: t -> h * product t

(* Exercise: concat*)
let rec concat lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ concat t

(* Exercise: patterns *)
let rec match_bigred lst =
  match lst with
  | [] -> false
  | "bigred" :: _ -> true
  | _ :: t -> match_bigred t

let match_2or4 lst =
  match lst with
  | [ _; _ ] | [ _; _; _; _ ] -> true
  | _ -> false

let match_first2 lst =
  match lst with
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
