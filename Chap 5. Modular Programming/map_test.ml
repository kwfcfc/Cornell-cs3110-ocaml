(* map_test.ml *)

open OUnit2
module M = Map.BstMap

let tests =
  "BstMap"
  >::: [
         ( "empty_lookup_raises" >:: fun _ ->
           assert_raises Not_found (fun () -> M.lookup 1 M.empty) );
         ( "insert_and_lookup" >:: fun _ ->
           let m = M.insert 1 "one" M.empty in
           assert_equal "one" (M.lookup 1 m) );
         ( "insert_two_keys" >:: fun _ ->
           let m = M.empty |> M.insert 1 "one" |> M.insert 2 "two" in
           assert_equal "one" (M.lookup 1 m);
           assert_equal "two" (M.lookup 2 m) );
         ( "insert_override" >:: fun _ ->
           let m = M.empty |> M.insert 1 "one" |> M.insert 1 "uno" in
           assert_equal "uno" (M.lookup 1 m) );
         ( "bindings_bst" >:: fun _ ->
           let m =
             M.empty
             |> M.insert 2 "two"
             |> M.insert 3 "tres"
             |> M.insert 1 "one"
           in
           let bindings = M.bindings m in
           assert_equal [ (1, "one"); (2, "two"); (3, "tres") ] bindings );
         ( "bindings_singleton" >:: fun _ ->
           let m = M.insert 1 "one" M.empty in
           assert_equal [ (1, "one") ] (M.bindings m) );
         ( "bindings_multiple" >:: fun _ ->
           let m = M.empty |> M.insert 1 "one" |> M.insert 2 "two" in
           let bindings = M.bindings m in
           assert_equal [ (1, "one"); (2, "two") ] bindings );
         ( "bindings_override" >:: fun _ ->
           let m = M.empty |> M.insert 1 "one" |> M.insert 1 "uno" in
           assert_equal [ (1, "uno") ] (M.bindings m) );
       ]
