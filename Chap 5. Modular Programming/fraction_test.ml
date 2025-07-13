open OUnit2
module F = Fraction.FractionInt

let tests_fraction =
  "Fraction"
  >::: [
         ( "make_normal" >:: fun _ ->
           let f = F.make 2 3 in
           assert_equal 2 (F.numerator f);
           assert_equal 3 (F.denominator f) );
         ( "make_zero_denominator_raises" >:: fun _ ->
           assert_raises Division_by_zero (fun () -> F.make 1 0) );
         ( "to_string" >:: fun _ ->
           let f = F.make 2 3 in
           assert_equal "2 / 3" (F.to_string f) );
         ( "to_float" >:: fun _ ->
           let f = F.make 1 2 in
           assert_equal 0.5 (F.to_float f) );
         ( "add" >:: fun _ ->
           let f1 = F.make 1 2 in
           let f2 = F.make 1 3 in
           let f3 = F.add f1 f2 in
           assert_equal ((1 * 3) - (1 * 2)) (F.numerator f3);
           assert_equal (2 * 3) (F.denominator f3) );
         ( "mul" >:: fun _ ->
           let f1 = F.make 2 5 in
           let f2 = F.make 3 7 in
           let f3 = F.mul f1 f2 in
           assert_equal (2 * 3) (F.numerator f3);
           assert_equal (5 * 7) (F.denominator f3) );
       ]

module FR = Fraction.FractionReduced

let tests_fraction_reduced =
  "FractionReduced"
  >::: [
         ( "make_and_reduce" >:: fun _ ->
           let f = FR.make 2 4 in
           assert_equal 1 (FR.numerator f);
           assert_equal 2 (FR.denominator f) );
         ( "negative_denominator" >:: fun _ ->
           let f = FR.make 1 (-3) in
           assert_equal (-1) (FR.numerator f);
           assert_equal 3 (FR.denominator f) );
         ( "zero_numerator" >:: fun _ ->
           let f = FR.make 0 5 in
           assert_equal 0 (FR.numerator f);
           assert_equal 1 (FR.denominator f) );
         ( "add_and_reduce" >:: fun _ ->
           let f1 = FR.make 1 6 in
           let f2 = FR.make 1 3 in
           let f3 = FR.add f1 f2 in
           (* (1/6) - (1/3) = (-1/6), reduced is -1/6 *)
           assert_equal (-1) (FR.numerator f3);
           assert_equal 6 (FR.denominator f3) );
         ( "mul_and_reduce" >:: fun _ ->
           let f1 = FR.make 2 4 in
           let f2 = FR.make 3 9 in
           let f3 = FR.mul f1 f2 in
           (* (2/4) * (3/9) = (6/36) = (1/6) *)
           assert_equal 1 (FR.numerator f3);
           assert_equal 6 (FR.denominator f3) );
       ]
