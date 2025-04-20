open OUnit2
open Exercises

let exercise1_expected = [ 1; 2; 3; 4; 5 ]

let sort_list_of_lists (lst : int list list) =
  let sorted_sublist = List.map (List.sort compare) lst in
  let rec compare_list (a : int list) (b : int list) =
    if List.length a = List.length b then
      match (a, b) with
      | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then compare_list t1 t2 else compare h1 h2
      | [], [] -> 0
      | _ -> compare (List.length a) (List.length b)
    else compare (List.length a) (List.length b)
  in
  List.sort compare_list sorted_sublist

(* Sample data for pokemon*)
let p1 = { name = "Charmander"; hp = 39; ptype = Fire }
let p2 = { name = "Bulbasaur"; hp = 45; ptype = Normal }
let p3 = { name = "Squirtle"; hp = 44; ptype = Water }

let tests =
  "test suite for exercises of Chapter 3"
  >::: [
         (*
            * Exercise list
         *)
         ( "bracket list 1" >:: fun _ ->
           assert_equal exercise1_expected bracket_list1 );
         ( "bracket list 2" >:: fun _ ->
           assert_equal exercise1_expected bracket_list2 );
         ( "bracket list 3" >:: fun _ ->
           assert_equal exercise1_expected bracket_list3 );
         (*
            * Exercise product
         *)
         ("product of empty" >:: fun _ -> assert_equal 1 (product []));
         ( "product of [2, 2, 7]" >:: fun _ ->
           assert_equal 28 (product [ 2; 2; 7 ]) );
         ( "product of [-2, 5, 11]" >:: fun _ ->
           assert_equal (-110) (product [ -2; 5; 11 ]) );
         (*
            * Exercise concat
         *)
         ("concat of empty" >:: fun _ -> assert_equal "" (concat []));
         ( "concat of [h; e; l; l; o]" >:: fun _ ->
           assert_equal "hello" (concat [ "h"; "e"; "l"; "l"; "o" ]) );
         (*
            * Exercise patterns
         *)
         ( "match bigred empty" >:: fun _ ->
           assert_bool "Empty list match fails" (not (match_bigred [])) );
         ( "match bigred no element" >:: fun _ ->
           assert_bool "No big red match fails"
             (not (match_bigred [ "bigblue"; "l"; "bigGreen" ])) );
         ( "match bigred found " >:: fun _ ->
           assert_bool "found big red"
             (match_bigred [ "bigblue"; "bigred"; "yellow" ]) );
         (* match list with 2 or 4 *)
         ( "match list with 2" >:: fun _ ->
           assert_bool "list of 2" (match_2or4 [ "bigblue"; "bigred" ]) );
         ( "match list with 4" >:: fun _ ->
           assert_bool "list of 4" (match_2or4 [ 8; 9; 6; 4 ]) );
         ( "match list with other" >:: fun _ ->
           assert_bool "list of 3" (not (match_2or4 [ 6; 1; 5 ])) );
         (* match list with the same first 2 elements *)
         ( "match list [1; 2]" >:: fun _ ->
           assert_bool "list of 2" (not (match_first2 [ 1; 2 ])) );
         ( "match list true" >:: fun _ ->
           assert_bool "list of 3" (match_first2 [ "a"; "a"; "g" ]) );
         ( "match list only 1 element" >:: fun _ ->
           assert_bool "list of 1" (not (match_first2 [ true ])) );
         (*
            * Exercise library
         *)
         ( "Find fifth in list too short" >:: fun _ ->
           assert_equal 0 (fifth [ 8; 9; 6; 4 ]) );
         ( "Find fifth successfully" >:: fun _ ->
           assert_equal 6 (fifth [ 1; 9; 8; 9; 6; 4 ]) );
         ( "Sort the numbers" >:: fun _ ->
           assert_equal [ 9; 8; 6; 4 ] (mysort [ 6; 4; 8; 9 ]) );
         (*
            * Exercise library puzzle
         *)
         ( "Take last element" >:: fun _ ->
           assert_equal "X" (last_in_list [ "M"; "D"; "J"; "H"; "X" ]) );
         ( "No zeroes found" >:: fun _ ->
           assert_equal false (any_zeros [ 8; 9; 6; 4 ]) );
         (* We assume the list are not empty to find zeroes in list*)
         ( "Found zero" >:: fun _ ->
           assert_equal true (any_zeros [ 1; 3; 0; 7; 4 ]) );
         (*
            * Exercise take drop
         *)
         ( "Take all" >:: fun _ ->
           assert_equal [ 6; 4; 8; 9 ] (take 6 [ 6; 4; 8; 9 ]) );
         ( "Take few" >:: fun _ ->
           assert_equal [ "x"; "l" ] (take 2 [ "x"; "l"; "j"; "d"; "m" ]) );
         (* test drop *)
         ( " Drop all" >:: fun _ ->
           assert_equal [] (drop 8 [ "x"; "xia"; "l"; "shang" ]) );
         ( " Drop a few" >:: fun _ ->
           assert_equal [ 8.9; 6.4 ] (drop 3 [ 1.8; 2.4; 5.2; 8.9; 6.4 ]) );
         ( "Take all long list" >:: fun _ ->
           assert_equal
             (List.init 1_000_000 Fun.id)
             (take_tail 1_000_020 (List.init 1_000_000 Fun.id)) );
         ( "Drop long list" >:: fun _ ->
           assert_equal [ 999_998; 999_999 ]
             (drop 999_998 (List.init 1_000_000 Fun.id)) );
         (*
            * Exercise unimodal
         *)
         ( "Unimodal for empty list" >:: fun _ ->
           assert_bool "Unimodal" (is_unimodal []) );
         ( "Unimodal for constant list" >:: fun _ ->
           assert_bool "Unimodal" (is_unimodal [ 3; 3; 3; 3; 3 ]) );
         ( "Unimodal with increasing and decreasing" >:: fun _ ->
           assert_bool "Unimodal up and down" (is_unimodal [ 8; 9; 6; 4 ]) );
         ( "Unimodal with increasing list" >:: fun _ ->
           assert_bool "Unimodal up" (is_unimodal [ 1; 8; 9; 12 ]) );
         ( "Unimodal with descreasing list" >:: fun _ ->
           assert_bool "Unimodal down" (is_unimodal [ 9; 8; 4; 2 ]) );
         ( "Not unimodal" >:: fun _ ->
           assert_bool "Not unimodal" (not (is_unimodal [ 1; 9; 8; 9 ])) );
         (*
            * Exercise powerset
         *)
         ( "Powerset on smal list" >:: fun _ ->
           assert_equal
             (sort_list_of_lists
                [
                  [ 8; 9; 6 ]; [ 8; 9 ]; [ 8; 6 ]; [ 9; 6 ]; [ 8 ]; [ 9 ]; [ 6 ];
                ])
             (sort_list_of_lists (powerset [ 8; 9; 6 ])) );
         (*
            * Exercise Student
         *)
         ( "create_student basic" >:: fun _ ->
           let s = create_student "Ada" "Lovelace" 4.0 in
           assert_equal "Ada" s.first_name;
           assert_equal "Lovelace" s.last_name;
           assert_equal 4.0 s.gpa );
         ( "get_name test" >:: fun _ ->
           let fname, lname = get_name my_student in
           assert_equal "Goba" fname;
           assert_equal "Lewis" lname );
         ( "equality test" >:: fun _ ->
           let s1 = create_student "A" "B" 3.1 in
           let s2 = { first_name = "A"; last_name = "B"; gpa = 3.1 } in
           assert_equal s1 s2 );
         (*
            * Exercise pokerecord
         *)
         ( "charizard data" >:: fun _ ->
           assert_equal "charizard" charizard.name;
           assert_equal 78 charizard.hp;
           assert_equal Fire charizard.ptype );
         ( "squirtle data" >:: fun _ ->
           assert_equal "squirtle" squirtle.name;
           assert_equal 44 squirtle.hp;
           assert_equal Water squirtle.ptype );
         ( "distinct poketype" >:: fun _ ->
           assert_bool "Fire ≠ Water" (Fire <> Water);
           assert_bool "Normal ≠ Fire" (Normal <> Fire) );
         (*
            * Exercise on safe hd and tl
         *)
         ( "safe_hd on non-empty list" >:: fun _ ->
           assert_equal (Some 1) (safe_hd [ 1; 2; 3 ]) );
         ("safe_hd on empty list" >:: fun _ -> assert_equal None (safe_hd []));
         ( "safe_tl on non-empty list" >:: fun _ ->
           assert_equal (Some [ 2; 3 ]) (safe_tl [ 1; 2; 3 ]) );
         ( "safe_tl on singleton list" >:: fun _ ->
           assert_equal (Some []) (safe_tl [ 42 ]) );
         ("safe_tl on empty list" >:: fun _ -> assert_equal None (safe_tl []));
         (*
            * Exercise find max hp in pokemon list
         *)
         ( "max_hp normal case" >:: fun _ ->
           let lst = [ p1; p2; p3 ] in
           let result = max_hp lst in
           assert_equal (Some p2) result );
         ( "max_hp with one element" >:: fun _ ->
           assert_equal (Some p3) (max_hp [ p3 ]) );
         ("max_hp with empty list" >:: fun _ -> assert_equal None (max_hp []));
         (* save the first greatest hp *)
         ( "max_hp with equal hp" >:: fun _ ->
           let a = { name = "A"; hp = 50; ptype = Normal } in
           let b = { name = "B"; hp = 50; ptype = Water } in
           assert_equal (Some a) (max_hp [ a; b ]) );
       ]

let _ = run_test_tt_main tests
