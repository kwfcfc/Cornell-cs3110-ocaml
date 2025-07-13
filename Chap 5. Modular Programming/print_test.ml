open OUnit2
open Print

let test_print_int _ =
  assert_equal "42" (PrintInt.print 42);
  assert_equal "0" (PrintInt.print 0)

let test_print_string _ =
  assert_equal "forty-two" (PrintString.print "forty-two");
  assert_equal "plastic" (PrintString.print "plastic")

let tests =
  "Print functor tests"
  >::: [ "print int" >:: test_print_int; "print string" >:: test_print_string ]
