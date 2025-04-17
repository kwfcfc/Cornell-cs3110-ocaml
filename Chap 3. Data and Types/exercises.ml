(* Exercise: list expressions *)
let bracket_list1 = [1; 2; 3; 4; 5]
let bracket_list2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
let bracket_list3 = [1] @ [2; 3; 4] @ [5]

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
  | [_; _] | [_; _; _; _] -> true
  | _ -> false

let match_first2 lst = 
  match lst with
  | first :: second :: _ -> first = second
  | _ -> false