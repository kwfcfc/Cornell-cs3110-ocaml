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

let tests =
  "test suite for exercises of Chapter 4" >::: [ repeat_tests; product_tests ]

let _ = run_test_tt_main tests
