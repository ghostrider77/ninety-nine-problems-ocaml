open OUnit2


let test_last _ =
  assert_equal None (Problems.last []);
  assert_equal (Some 3) (Problems.last [1; 2 ;3]);
  assert_equal (Some 'b') (Problems.last ['a'; 'b'])


let test_last_two _ =
  assert_equal None (Problems.last_two ["hello"]);
  assert_equal None (Problems.last_two []);
  assert_equal (Some (2, 3)) (Problems.last_two [1; 2; 3])


let test_at _ =
  assert_equal None (Problems.at (-1) [1; 2; 3]);
  assert_equal None (Problems.at 2 [1; 2]);
  assert_equal (Some 3) (Problems.at 2 [1; 2 ;3]);
  assert_equal (Some 'b') (Problems.at 1 ['a'; 'b'; 'c'])


let suite =
  "Problems Tests" >::: [
    "test_last" >:: test_last;
    "test_last_two" >:: test_last_two;
    "test_at" >:: test_at
  ]

let () =
  run_test_tt_main suite
