(* map_test.ml *)

open OUnit2
module M = Map.BstMap

let bst_map_tests =
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

(* Exercise: use char map *)
module C = Map.CharMap

let use_map =
  C.(
    empty
    |> add 'A' "Alpha"
    |> add 'E' "Echo"
    |> add 'S' "Sierra"
    |> add 'V' "Victor")

let test_find_E _ = assert_equal "Echo" (C.find 'E' use_map)

let test_remove_A_mem _ =
  let use_map_remove = C.remove 'A' use_map in
  assert_equal false (C.mem 'A' use_map_remove)

let test_bindings_after_remove_A _ =
  let use_map_remove = C.remove 'A' use_map in
  let bindings = C.bindings use_map_remove in
  assert_equal [ ('E', "Echo"); ('S', "Sierra"); ('V', "Victor") ] bindings

(* Exercise: bindings *)

(* All three bindings are the same, because the assoc list is ordered by key. *)
let test_three_bindings _ =
  let map1_bindings = C.(empty |> add 'x' 0 |> add 'y' 1 |> bindings) in
  let map2_bindings = C.(empty |> add 'y' 1 |> add 'x' 0 |> bindings) in
  let map3_bindings =
    C.(empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings)
  in
  assert_equal map2_bindings map3_bindings;
  assert_equal map1_bindings map3_bindings;
  assert_equal [ ('x', 0); ('y', 1) ] map3_bindings

let char_map_tests =
  "CharMap tests"
  >::: [
         "find_E" >:: test_find_E;
         "remove_A_mem" >:: test_remove_A_mem;
         "bindings_after_remove_A" >:: test_bindings_after_remove_A;
         "bindings_of_three_maps" >:: test_three_bindings;
       ]

(* Date and Calendar tests *)
type d = Map.date

module D = Map.Date
module DMap = Map.DateMap

let date_jan1 : d = { month = 1; day = 1 }
let date_jan2 : d = { month = 1; day = 2 }
let date_feb1 : d = { month = 2; day = 1 }

let test_date_compare_equal _ =
  assert_equal 0 (D.compare date_jan1 { month = 1; day = 1 })

let test_date_compare_lt _ =
  assert_bool "jan1 < jan2" (D.compare date_jan1 date_jan2 < 0);
  assert_bool "jan2 < feb1" (D.compare date_jan2 date_feb1 < 0)

let test_date_compare_gt _ =
  assert_bool "feb1 > jan2" (D.compare date_feb1 date_jan2 > 0)

let calendar =
  DMap.(
    empty
    |> add date_jan1 "New Year's Day"
    |> add date_jan2 "Second Day"
    |> add date_feb1 "February Start")

let test_calendar_lookup _ =
  assert_equal "New Year's Day" (DMap.find date_jan1 calendar);
  assert_equal "Second Day" (DMap.find date_jan2 calendar);
  assert_equal "February Start" (DMap.find date_feb1 calendar)

let test_calendar_remove _ =
  let cal = DMap.remove date_jan2 calendar in
  assert_bool "jan2 removed" (not (DMap.mem date_jan2 cal));
  assert_equal "New Year's Day" (DMap.find date_jan1 cal)

let test_calendar_bindings_order _ =
  let open Map in
  let bindings = DMap.bindings calendar in
  assert_equal
    [
      ({ month = 1; day = 1 }, "New Year's Day");
      ({ month = 1; day = 2 }, "Second Day");
      ({ month = 2; day = 1 }, "February Start");
    ]
    bindings

let date_tests =
  "Date and Calendar"
  >::: [
         "date_compare_equal" >:: test_date_compare_equal;
         "date_compare_lt" >:: test_date_compare_lt;
         "date_compare_gt" >:: test_date_compare_gt;
         "calendar_lookup" >:: test_calendar_lookup;
         "calendar_remove" >:: test_calendar_remove;
         "calendar_bindings_order" >:: test_calendar_bindings_order;
       ]
