open OUnit2
open Exercises

let test_zero _ =
  let open Complex in
  assert_equal zero (0., 0.)

let test_add _ =
  let open Complex in
  assert_equal (add (1., 2.) (3., 4.)) (4., 6.)

let module_complex_tests =
  "Complex tests" >::: [ "zero" >:: test_zero; "add" >:: test_add ]

let tests =
  "test suite for exercises of Chapter 5"
  >::: [
         module_complex_tests;
         Map_test.tests;
         Fraction_test.tests_fraction;
         Fraction_test.tests_fraction_reduced;
       ]

let _ = run_test_tt_main tests
