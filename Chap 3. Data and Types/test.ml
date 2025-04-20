open OUnit2
open Exercises

let exercise1_expected = [ 1; 2; 3; 4; 5 ]

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
       ]

let _ = run_test_tt_main tests
