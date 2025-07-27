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


let test_length _ =
  assert_equal 0 (Problems.length []);
  assert_equal 3 (Problems.length ["a"; "b"; "c"]);
  assert_equal 100 (Problems.length (List.init 100 Fun.id))


let test_rev _ =
  assert_equal [] (Problems.rev []);
  assert_equal ["c"; "b"; "a"] (Problems.rev ["a"; "b"; "c"])


let test_is_palindrome _ =
  assert_equal true (Problems.is_palindrome []);
  assert_equal false (Problems.is_palindrome ['a'; 'b'; 'c']);
  assert_equal true (Problems.is_palindrome [9; 8; 7; 8; 9])


let suite =
  "Problems Tests" >::: [
    "test_last" >:: test_last;
    "test_last_two" >:: test_last_two;
    "test_at" >:: test_at;
    "test_length" >:: test_length;
    "test_rev" >:: test_rev;
    "test_is_palindrome" >:: test_is_palindrome
  ]

let () =
  run_test_tt_main suite
