(* set_test.ml *)

open OUnit2
module S = Set.InsensitiveSet

let use_set = S.(empty |> add "Agr" |> add "grr" |> add "aRgh")
let test_find_agr _ = assert_equal "Agr" (S.find "aGr" use_set)
let test_not_find _ = assert_raises Not_found (fun () -> S.find "aBr" use_set)

let test_not_add _ =
  let use_set_not_added = S.add "gRR" use_set in
  assert_equal use_set_not_added use_set

let set_tests =
  "set tests"
  >::: [
         "find_agr" >:: test_find_agr;
         "not_found" >:: test_not_find;
         "set_not_added" >:: test_not_add;
       ]
