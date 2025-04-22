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

(* Tree test data *)
let t_leaf = Leaf
let t_single = Node (1, Leaf, Leaf)
let t_single_b = Node (99, Leaf, Leaf)
let t_left = Node (1, Node (2, Leaf, Leaf), Leaf)
let t_right = Node (1, Leaf, Node (2, Leaf, Leaf))
let t_balanced = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))

let t_unbalanced =
  Node (1, Node (2, Leaf, Leaf), Node (3, Node (4, Leaf, Leaf), Leaf))

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
           (* Sample data for pokemon*)
           let p1 = { name = "Charmander"; hp = 39; ptype = Fire } in
           let p2 = { name = "Bulbasaur"; hp = 45; ptype = Normal } in
           let p3 = { name = "Squirtle"; hp = 44; ptype = Water } in
           let lst = [ p1; p2; p3 ] in
           let result = max_hp lst in
           assert_equal (Some p2) result );
         ( "max_hp with one element" >:: fun _ ->
           let p3 = { name = "Squirtle"; hp = 44; ptype = Water } in
           assert_equal (Some p3) (max_hp [ p3 ]) );
         ("max_hp with empty list" >:: fun _ -> assert_equal None (max_hp []));
         (* save the first greatest hp *)
         ( "max_hp with equal hp" >:: fun _ ->
           let a = { name = "A"; hp = 50; ptype = Normal } in
           let b = { name = "B"; hp = 50; ptype = Water } in
           assert_equal (Some a) (max_hp [ a; b ]) );
         (*
            * Exercise date before
         *)
         ( "year before" >:: fun _ ->
           assert_bool "year is before" (is_before (2020, 5, 10) (2021, 1, 1))
         );
         ( "same year, month before" >:: fun _ ->
           assert_bool "month is before" (is_before (2023, 3, 1) (2023, 4, 1))
         );
         ( "same year & month, day before" >:: fun _ ->
           assert_bool "day is before" (is_before (2023, 4, 1) (2023, 4, 2)) );
         ( "same exact date" >:: fun _ ->
           assert_bool "same day, not before"
             (not (is_before (2023, 4, 2) (2023, 4, 2))) );
         ( "later date returns false" >:: fun _ ->
           assert_bool "later day, not before"
             (not (is_before (2024, 1, 1) (2023, 12, 31))) );
         (*
            * Exercise earliest date
         *)
         ( "earliest of multiple dates" >:: fun _ ->
           let d1 = (2023, 5, 10) in
           let d2 = (2020, 1, 1) in
           let d3 = (2021, 12, 31) in
           assert_equal (Some d2) (earliest [ d1; d2; d3 ]) );
         ( "earliest of same date repeated" >:: fun _ ->
           let d = (2022, 2, 2) in
           assert_equal (Some d) (earliest [ d; d; d ]) );
         ( "earliest from one element" >:: fun _ ->
           let d = (2000, 1, 1) in
           assert_equal (Some d) (earliest [ d ]) );
         ( "earliest from empty list" >:: fun _ ->
           assert_equal None (earliest []) );
         ( "earliest with closest day" >:: fun _ ->
           let d1 = (2023, 5, 10) in
           let d2 = (2023, 5, 9) in
           assert_equal (Some d2) (earliest [ d1; d2 ]) );
         (* Exercise * assoc list
         *)
         ( "find 2 in simple map" >:: fun _ ->
           assert_equal (Some "two") two_in_simple_map );
         ( "4 not found in simple map" >:: fun _ ->
           assert_equal None four_in_simple_map );
         (*
            * Exercise card
         *)
         ("card1 is Club Ace" >:: fun _ -> assert_equal (Club, Ace) card1);
         ( "card2 rank is Queen" >:: fun _ ->
           let _, r = card2 in
           assert_equal Queen r );
         ( "card3 suit is Diamond" >:: fun _ ->
           let s, _ = card3 in
           assert_equal Diamond s );
         ( "card4 pattern match" >:: fun _ ->
           match card4 with
           | Spade, Seven -> assert_bool "Matched Spade Seven" true
           | _ -> assert_failure "card4 did not match Spade Seven" );
         (*
            * Exercise match
         *)
         ( "match1 matches Some x :: tl" >:: fun _ ->
           match match1 with
           | Some x :: tl ->
             ignore x;
             ignore tl;
             assert_failure "Should not match Some x :: tl" (* matched*)
           | _ -> () );
         ( "match2 matches [Some 3110; None]" >:: fun _ ->
           match match2 with
           | [ Some 3110; None ] ->
             assert_failure "Should not match [Some 3110; None]" (* matched *)
           | _ -> () );
         ( "match3 matches [Some x; _]" >:: fun _ ->
           match match3 with
           | [ Some x; _ ] ->
             ignore x;
             assert_failure "Should not match [Some x; _]" (* matched *)
           | _ -> () );
         ( "match4 matches h1 :: h2 :: tl" >:: fun _ ->
           match match4 with
           | h1 :: h2 :: tl ->
             ignore h1;
             ignore h2;
             ignore tl;
             assert_failure "Should not match h1 :: h2 :: tl" (* matched *)
           | _ -> () );
         ( "match5 matches h :: tl" >:: fun _ ->
           match match5 with
           | h :: tl ->
             ignore h;
             ignore tl;
             assert_failure "Did not match h :: tl" (* matched *)
           | _ -> () );
         (*
            * Exercise Quadrant
         *)
         ( "Point in quadrant I" >:: fun _ ->
           assert_equal (Some I) (quadrant (6, 4)) );
         ( "Point in quadrant II" >:: fun _ ->
           assert_equal (Some II) (quadrant (-2, 5)) );
         ( "Point in quadrant III" >:: fun _ ->
           assert_equal (Some III) (quadrant (-3, -4)) );
         ( "Point in quadrant IV" >:: fun _ ->
           assert_equal (Some IV) (quadrant (6, -7)) );
         ( "Point on x-axis (y = 0)" >:: fun _ ->
           assert_equal None (quadrant (-8, 0)) );
         ( "Point on y-axis (x = 0)" >:: fun _ ->
           assert_equal None (quadrant (0, 9)) );
         ( "Point at origin (0, 0)" >:: fun _ ->
           assert_equal None (quadrant (0, 0)) );
         (*
            * Exercise Quadrant_when
         *)
         ( "Point in quadrant I" >:: fun _ ->
           assert_equal (Some I) (quadrant_when (6, 4)) );
         ( "Point in quadrant II" >:: fun _ ->
           assert_equal (Some II) (quadrant_when (-2, 5)) );
         ( "Point in quadrant III" >:: fun _ ->
           assert_equal (Some III) (quadrant_when (-3, -4)) );
         ( "Point in quadrant IV" >:: fun _ ->
           assert_equal (Some IV) (quadrant_when (6, -7)) );
         ( "Point on x-axis (y = 0)" >:: fun _ ->
           assert_equal None (quadrant_when (-8, 0)) );
         ( "Point on y-axis (x = 0)" >:: fun _ ->
           assert_equal None (quadrant_when (0, 9)) );
         ( "Point at origin (0, 0)" >:: fun _ ->
           assert_equal None (quadrant_when (0, 0)) );
         (*
            * Exercise depth (of tree)
         *)
         ("depth of Leaf is 0" >:: fun _ -> assert_equal 0 (depth Leaf));
         ( "depth of single node is 1" >:: fun _ ->
           let t = Node (42, Leaf, Leaf) in
           assert_equal 1 (depth t) );
         ( "depth of left-heavy tree" >:: fun _ ->
           let t = Node (1, Node (2, Node (3, Leaf, Leaf), Leaf), Leaf) in
           assert_equal 3 (depth t) );
         ( "depth of right-heavy tree" >:: fun _ ->
           let t = Node (1, Leaf, Node (2, Leaf, Node (3, Leaf, Leaf))) in
           assert_equal 3 (depth t) );
         ( "depth of full binary tree" >:: fun _ ->
           let t = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf)) in
           assert_equal 2 (depth t) );
         ( "depth of deeper balanced tree" >:: fun _ ->
           let t =
             Node
               ( 0,
                 Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf)),
                 Node (4, Node (5, Leaf, Leaf), Node (6, Leaf, Leaf)) )
           in
           assert_equal 3 (depth t) );
         (*
            * Exercise same_shape
         *)
         ( "two empty trees" >:: fun _ ->
           assert_bool "Leaf vs Leaf" (same_shape t_leaf t_leaf) );
         ( "identical single-node trees" >:: fun _ ->
           assert_bool "single vs single" (same_shape t_single t_single_b) );
         ( "leaf vs non-leaf" >:: fun _ ->
           assert_equal false (same_shape t_leaf t_single) );
         ( "left vs right heavy trees (shape different)" >:: fun _ ->
           assert_equal false (same_shape t_left t_right) );
         ( "two identical balanced trees" >:: fun _ ->
           assert_equal true (same_shape t_balanced t_balanced) );
         ( "balanced vs unbalanced tree" >:: fun _ ->
           assert_equal false (same_shape t_balanced t_unbalanced) );
         (*
            * Exercise list max exn
         *)
         ( "max of ascending list" >:: fun _ ->
           assert_equal 3 (list_max [ 1; 2; 3 ]) );
         ( "max of descending list" >:: fun _ ->
           assert_equal 5 (list_max [ 5; 4; 3 ]) );
         ( "max of negative numbers" >:: fun _ ->
           assert_equal (-1) (list_max [ -5; -1; -10 ]) );
         ("max of singleton list" >:: fun _ -> assert_equal 42 (list_max [ 42 ]));
         ( "empty list should raise" >:: fun _ ->
           assert_raises (Failure "list_max") (fun () -> list_max []) );
         (*
            * Exercise list max string
         *)
         ( "non-empty list returns max as string" >:: fun _ ->
           assert_equal "3" (list_max_string [ 1; 2; 3 ]) );
         ( "single element list" >:: fun _ ->
           assert_equal "42" (list_max_string [ 42 ]) );
         ( "all negative values" >:: fun _ ->
           assert_equal "-1" (list_max_string [ -10; -1; -5 ]) );
         ( "empty list returns 'empty'" >:: fun _ ->
           assert_equal "empty" (list_max_string []) );
         ( "empty tree is BST" >:: fun _ ->
           assert_bool "Leaf is BST" (is_bst Leaf) );
         (*
            * Exercise is_bst, valid Binary Search Tree
         *)
         ( "valid BST" >:: fun _ ->
           let t =
             Node
               ( (5, "root"),
                 Node ((3, "left"), Leaf, Leaf),
                 Node ((7, "right"), Leaf, Leaf) )
           in
           assert_equal true (is_bst t) );
         ( "invalid BST (wrong left)" >:: fun _ ->
           let t =
             Node
               ( (5, "root"),
                 Node ((6, "wrong"), Leaf, Leaf),
                 (* 6 > 5, wrong left side *)
                 Node ((7, "right"), Leaf, Leaf) )
           in
           assert_equal false (is_bst t) );
         ( "invalid BST (wrong right)" >:: fun _ ->
           let t =
             Node
               ( (5, "root"),
                 Node ((3, "left"), Leaf, Leaf),
                 Node ((2, "wrong"), Leaf, Leaf) (* 2 < 5, wrong right side *)
               )
           in
           assert_equal false (is_bst t) );
         ( "nested valid BST" >:: fun _ ->
           let t =
             Node
               ( (10, "root"),
                 Node
                   ( (5, "left"),
                     Node ((2, "ll"), Leaf, Leaf),
                     Node ((7, "lr"), Leaf, Leaf) ),
                 Node
                   ( (15, "right"),
                     Node ((13, "rl"), Leaf, Leaf),
                     Node ((17, "rr"), Leaf, Leaf) ) )
           in
           assert_equal true (is_bst t) );
         ("Quadrant I" >:: fun _ -> assert_equal (Some `I) (quadrant_poly 1 1));
         (*
            * Exercise quadrant_poly
         *)
         ( "Quadrant II" >:: fun _ ->
           assert_equal (Some `II) (quadrant_poly (-1) 5) );
         ( "Quadrant III" >:: fun _ ->
           assert_equal (Some `III) (quadrant_poly (-1) (-3)) );
         ( "Quadrant IV" >:: fun _ ->
           assert_equal (Some `IV) (quadrant_poly 4 (-2)) );
         ("On x-axis" >:: fun _ -> assert_equal None (quadrant_poly 5 0));
         ("On y-axis" >:: fun _ -> assert_equal None (quadrant_poly 0 2));
         ("Origin" >:: fun _ -> assert_equal None (quadrant_poly 0 0));
       ]

let _ = run_test_tt_main tests
