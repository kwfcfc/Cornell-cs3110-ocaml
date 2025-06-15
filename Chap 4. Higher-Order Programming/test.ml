open OUnit2
open Exercises

let float_eq ?(eps = 1e-9) a b = abs_float (a -. b) < eps

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

let tests =
  "test suite for exercises of Chapter 4"
  >::: [ repeat_tests; product_tests; exists_tests; balance_tests ]

let _ = run_test_tt_main tests
