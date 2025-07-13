open OUnit2
open Exercises

let float_eq ?(eps = 1e-9) a b = abs_float (a -. b) < eps
let unordered_eq l1 l2 = List.sort compare l1 = List.sort compare l2

let assert_unordered_equal l1 l2 =
  assert_bool
    ("Expected: " ^ String.concat "," l1 ^ " but got: " ^ String.concat "," l2)
    (unordered_eq l1 l2)

let repeat_tests =
  "test repeat"
  >::: [
         ( "repeat double 3 1" >:: fun _ ->
           assert_equal 8 (repeat (fun x -> x * 2) 3 1) );
         ( "repeat succ 4 1" >:: fun _ ->
           assert_equal 5 (repeat (fun x -> x + 1) 4 1) );
         ( "repeat square 2 2" >:: fun _ ->
           assert_equal 16 (repeat (fun x -> x * x) 2 2) );
         ( "repeat zero" >:: fun _ ->
           assert_equal 7 (repeat (fun x -> x + 1) 0 7) );
       ]

let product_tests =
  "test product"
  >::: [
         ( "product_left" >:: fun _ ->
           assert_bool "empty" (float_eq (product_left []) 1.);
           assert_bool "single" (float_eq (product_left [ 2.0 ]) 2.0);
           assert_bool "multi" (float_eq (product_left [ 2.0; 3.0; 4.0 ]) 24.0)
         );
         ( "product_right" >:: fun _ ->
           assert_bool "empty" (float_eq (product_left []) 1.);
           assert_bool "single" (float_eq (product_left [ 3.14 ]) 3.14);
           assert_bool "multi"
             (float_eq (product_left [ 3.14; 2.718; 0.618 ]) 5.27433336) );
       ]

let exists_testsets make_exists =
  let even x = x mod 2 = 0 in
  let pos x = x > 0 in
  let allneg x = x < 0 in
  [
    ("empty" >:: fun _ -> assert_equal false (make_exists even []));
    ("one true" >:: fun _ -> assert_equal true (make_exists even [ 1; 2; 3 ]));
    ("all false" >:: fun _ -> assert_equal false (make_exists even [ 1; 3; 5 ]));
    ("all true" >:: fun _ -> assert_equal true (make_exists pos [ 1; 2; 3 ]));
    ( "one element true" >:: fun _ ->
      assert_equal true (make_exists allneg [ -3 ]) );
    ( "one element false" >:: fun _ ->
      assert_equal false (make_exists allneg [ 0 ]) );
    ( "mixed sign" >:: fun _ ->
      assert_equal true (make_exists allneg [ -2; 0; 3 ]) );
  ]

let exists_tests =
  "exists"
  >::: [
         "exists_rec" >::: exists_testsets exists_rec;
         "exists_fold" >::: exists_testsets exists_fold;
         "exists_lib" >::: exists_testsets exists_lib;
       ]

let balance_testsets balance =
  [
    ("empty" >:: fun _ -> assert_bool "empty" (float_eq (balance 10.0 []) 10.0));
    ( "single" >:: fun _ ->
      assert_bool "single" (float_eq (balance 10.0 [ 1.0 ]) 9.0) );
    ( "multi" >:: fun _ ->
      assert_bool "multi" (float_eq (balance 20.0 [ 2.0; 3.0; 5.0 ]) 10.0) );
    ( "negative" >:: fun _ ->
      assert_bool "negative" (float_eq (balance 5.0 [ -2.0; 1.0 ]) 6.0) );
    ( "zero" >:: fun _ ->
      assert_bool "zero" (float_eq (balance 0.0 [ 1.0; 2.0; 3.0 ]) (-6.0)) );
  ]

let balance_tests =
  "balance"
  >::: [
         "balance_left"
         >::: balance_testsets (fun acc lst -> balance_left acc lst);
         "balance_right" >::: balance_testsets balance_right;
         "balance_rec" >::: balance_testsets balance_rec;
       ]

let uncurried_append_testsets _ =
  let cases =
    [
      ([], [], []);
      ([ 1; 2 ], [], [ 1; 2 ]);
      ([], [ 3; 4 ], [ 3; 4 ]);
      ([ 1 ], [ 2 ], [ 1; 2 ]);
      ([ 1; 2 ], [ 3; 4 ], [ 1; 2; 3; 4 ]);
    ]
  in
  List.iter
    (fun (a, b, expected) ->
       assert_equal (uncurried_append (a, b)) (List.append a b);
       assert_equal (uncurried_append (a, b)) expected)
    cases

let uncurried_compare_testsets _ =
  let cases =
    [
      ('a', 'b', Char.compare 'a' 'b');
      ('z', 'a', Char.compare 'z' 'a');
      ('c', 'c', Char.compare 'c' 'c');
    ]
  in
  List.iter
    (fun (a, b, expected) -> assert_equal (uncurried_compare (a, b)) expected)
    cases

let uncurried_max_testsets _ =
  let cases =
    [
      (1, 2, Stdlib.max 1 2);
      (3, -1, Stdlib.max 3 (-1));
      (5, 5, Stdlib.max 5 5);
      (-2, -3, Stdlib.max (-2) (-3));
    ]
  in
  List.iter
    (fun (a, b, expected) -> assert_equal (uncurried_max (a, b)) expected)
    cases

let uncurried_tests =
  "uncurried"
  >::: [
         "uncurried_append" >:: uncurried_append_testsets;
         "uncurried_compare" >:: uncurried_compare_testsets;
         "uncurried_max" >:: uncurried_max_testsets;
       ]

let map_composition_testsets (lst : 'a list) =
  List.iter
    (fun (lst, g, f) ->
       let want = List.map f (List.map g lst) in
       let got = map_composition f g lst in
       assert_equal want got)
    lst

let map_composition_tests =
  "map_composition"
  >::: [
         ( "int_to_int" >:: fun _ ->
           map_composition_testsets
             [
               ([], (fun x -> x + 1), fun x -> x * 2);
               ([ 1 ], (fun x -> x + 1), fun x -> x * 2);
               ([ 1; 2; 3 ], (fun x -> x + 1), fun x -> x * 2);
             ] );
         ( "int_to_string" >:: fun _ ->
           map_composition_testsets
             [ ([ 1; 2; 3 ], string_of_int, String.length) ] );
         ( "string_to_int" >:: fun _ ->
           map_composition_testsets
             [ ([ "a"; "bb"; "ccc" ], String.length, fun x -> x * 10) ] );
       ]

let test_greater3 _ =
  assert_equal [] (greater3 []);
  assert_equal [] (greater3 [ "a"; "be"; "cat" ]);
  assert_equal [ "four" ] (greater3 [ "one"; "two"; "four" ]);
  assert_equal [ "four"; "hello" ] (greater3 [ "one"; "four"; "hi"; "hello" ]);
  assert_equal [ "goodbye"; "hello" ]
    (greater3 [ "hi"; "goodbye"; "hello"; "no" ])

let test_add1f _ =
  let float_list_eq =
    assert_equal
      ~printer:(fun xs -> String.concat ";" (List.map string_of_float xs))
      ~cmp:(fun expected actual -> List.for_all2 float_eq expected actual)
  in
  float_list_eq [] (add1f []);
  float_list_eq [ 2.0 ] (add1f [ 1.0 ]);
  float_list_eq [ 2.5; -1.0; 4.0 ] (add1f [ 1.5; -2.0; 3.0 ]);
  float_list_eq [ 1.0 ] (add1f [ 0.0 ])

let test_join _ =
  assert_equal "" (join [] ",");
  assert_equal "hi" (join [ "hi" ] ",");
  assert_equal "hi,bye" (join [ "hi"; "bye" ] ",");
  assert_equal "one,two,three" (join [ "one"; "two"; "three" ] ",");
  assert_equal "ab--cd--ef" (join [ "ab"; "cd"; "ef" ] "--");
  assert_equal "a" (join [ "a" ] "|");
  assert_equal "" (join [] "|")

let more_list_fun_tests =
  "More list functions"
  >::: [
         "filter list with string longer than 3" >:: test_greater3;
         "add one to float list" >:: test_add1f;
         "join" >:: test_join;
       ]

let keys_testsets =
  [
    ( "string keys" >:: fun _ ->
      assert_unordered_equal (keys []) [];
      assert_unordered_equal (keys [ ("a", 1) ]) [ "a" ];
      assert_unordered_equal (keys [ ("a", 1); ("b", 2) ]) [ "a"; "b" ];
      assert_unordered_equal (keys [ ("a", 1); ("a", 2); ("a", 3) ]) [ "a" ];
      assert_unordered_equal
        (keys [ ("a", 1); ("b", 2); ("a", 3); ("c", 1); ("b", 7) ])
        [ "a"; "b"; "c" ];
      assert_unordered_equal
        (keys [ ("a", 1); ("b", 2); ("c", 3); ("d", 4) ])
        [ "a"; "b"; "c"; "d" ] );
    ( "int keys" >:: fun _ ->
      let mapped_expected, mapped_actual =
        ( [ 1; 2; 3 ],
          [ (1, "a"); (2, "b"); (1, "c"); (3, "d"); (2, "e") ] |> keys )
        |> fun (a, b) -> (List.map string_of_int a, List.map string_of_int b)
      in
      assert_unordered_equal mapped_actual mapped_expected );
    (* test empty keys *)
    ( "empty keys" >:: fun _ ->
      assert_unordered_equal (keys [ ("", 1); ("", 2); ("x", 3) ]) [ ""; "x" ]
    );
  ]

let keys_test = "assoc_keys" >::: keys_testsets

let is_valid_matrix_tests =
  "test_is_valid_matrix"
  >::: [
         ( "empty list is invalid" >:: fun _ ->
           assert_equal false (is_valid_matrix []) );
         ( "singleton empty row is not valid" >:: fun _ ->
           assert_equal false (is_valid_matrix [ [] ]) );
         ( "singleton nonempty row is valid" >:: fun _ ->
           assert_equal true (is_valid_matrix [ [ 1 ] ]) );
         ( "different length rows are invalid" >:: fun _ ->
           assert_equal false (is_valid_matrix [ [ 1; 2 ]; [ 3 ] ]) );
         ( "same length rows are valid" >:: fun _ ->
           assert_equal true (is_valid_matrix [ [ 1; 1; 1 ]; [ 9; 8; 7 ] ]) );
         ( "all empty rows is not valid" >:: fun _ ->
           assert_equal false (is_valid_matrix [ []; [] ]) );
         ( "row with zero columns and one with columns is invalid" >:: fun _ ->
           assert_equal false (is_valid_matrix [ []; [ 1 ] ]) );
         ( "more than two rows same length is valid" >:: fun _ ->
           assert_equal true (is_valid_matrix [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ])
         );
       ]

let add_row_vectors_tests =
  "test_add_row_vectors"
  >::: [
         ( "add simple vectors" >:: fun _ ->
           assert_equal [ 10; 9; 8 ] (add_row_vectors [ 1; 1; 1 ] [ 9; 8; 7 ])
         );
         ( "add zeros" >:: fun _ ->
           assert_equal [ 0; 0; 0 ] (add_row_vectors [ 0; 0; 0 ] [ 0; 0; 0 ]) );
         ( "add with negative numbers" >:: fun _ ->
           assert_equal [ 0; 1; 2 ] (add_row_vectors [ 1; 2; 3 ] [ -1; -1; -1 ])
         );
         ( "add with empty vectors" >:: fun _ ->
           assert_equal [] (add_row_vectors [] []) );
         (* The following test expects an exception (as OCaml's List.map2 raises
            Invalid_argument if vectors differ in length) *)
         ( "different lengths should raise" >:: fun _ ->
           assert_raises (Invalid_argument "List.map2") (fun () ->
             add_row_vectors [ 1; 2 ] [ 3 ]) );
       ]

let add_matrix_tests =
  "test_add_matrices"
  >::: [
         ( "add simple matrices" >:: fun _ ->
           let a = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] in
           let b = [ [ 10; 20; 30 ]; [ 40; 50; 60 ] ] in
           let expected = [ [ 11; 22; 33 ]; [ 44; 55; 66 ] ] in
           assert_equal expected (add_matrices a b) );
         ( "add matrices with negative numbers" >:: fun _ ->
           let a = [ [ 1; -2; 3 ]; [ -4; 5; -6 ] ] in
           let b = [ [ -1; 2; -3 ]; [ 4; -5; 6 ] ] in
           let expected = [ [ 0; 0; 0 ]; [ 0; 0; 0 ] ] in
           assert_equal expected (add_matrices a b) );
         ( "add two identity matrices" >:: fun _ ->
           assert_equal [ [ 2 ] ] (add_matrices [ [ 1 ] ] [ [ 1 ] ]) );
         (* Test exception for different number of rows *)
         ( "different number of rows should raise" >:: fun _ ->
           let a = [ [ 1; 2 ]; [ 3; 4 ] ] in
           let b = [ [ 5; 6 ] ] in
           assert_raises (Invalid_argument "List.map2") (fun () ->
             add_matrices a b) );
         (* Test exception for rows of different length *)
         ( "different row lengths should raise" >:: fun _ ->
           let a = [ [ 1; 2; 3 ]; [ 4; 5 ] ] in
           let b = [ [ 6; 7 ]; [ 8; 9; 10 ] ] in
           assert_raises (Invalid_argument "List.map2") (fun () ->
             add_matrices a b) );
       ]

let matrix_multiply_tests =
  "test_multiply_matrices"
  >::: [
         ( "simple 2x2 x 2x2" >:: fun _ ->
           let a = [ [ 1; 2 ]; [ 3; 4 ] ] in
           let b = [ [ 5; 6 ]; [ 7; 8 ] ] in
           let expected = [ [ 19; 22 ]; [ 43; 50 ] ] in
           assert_equal expected (multiply_matrices a b) );
         ( "2x3 x 3x2" >:: fun _ ->
           let a = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] in
           let b = [ [ 7; 8 ]; [ 9; 10 ]; [ 11; 12 ] ] in
           let expected = [ [ 58; 64 ]; [ 139; 154 ] ] in
           assert_equal expected (multiply_matrices a b) );
         ( "identity matrix" >:: fun _ ->
           let a = [ [ 1; 0 ]; [ 0; 1 ] ] in
           let id = [ [ 1; 0 ]; [ 0; 1 ] ] in
           assert_equal a (multiply_matrices a id) );
         ( "1x3 x 3x1" >:: fun _ ->
           let a = [ [ 1; 2; 3 ] ] in
           let b = [ [ 4 ]; [ 5 ]; [ 6 ] ] in
           let expected = [ [ 32 ] ] in
           assert_equal expected (multiply_matrices a b) );
         ( "different sizes should raise" >:: fun _ ->
           let a = [ [ 1; 2 ]; [ 3; 4 ] ] in
           let b = [ [ 5; 6; 7 ] ] in
           assert_raises (Invalid_argument "List.fold_left2") (fun () ->
             multiply_matrices a b) );
       ]

let tests =
  "test suite for exercises of Chapter 4"
  >::: [
         repeat_tests;
         product_tests;
         exists_tests;
         balance_tests;
         uncurried_tests;
         map_composition_tests;
         more_list_fun_tests;
         keys_test;
         is_valid_matrix_tests;
         add_row_vectors_tests;
         add_matrix_tests;
         matrix_multiply_tests;
       ]

let _ = run_test_tt_main tests
