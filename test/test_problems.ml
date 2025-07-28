open Ninety_nine_ocaml
open Ninety_nine_ocaml.Types
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


let test_flatten _ =
  assert_equal [] (Problems.flatten []);
  assert_equal [1; 2] (Problems.flatten [One 1; One 2]);
  assert_equal ["a"; "b"; "c"; "d"; "e"] (Problems.flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]])


let test_compress _ =
  assert_equal [] (Problems.compress []);
  assert_equal [1; 2; 3] (Problems.compress [1; 2; 3]);
  assert_equal ['a'; 'b'; 'c'] (Problems.compress ['a'; 'b'; 'b'; 'b'; 'c']);
  assert_equal ["a"; "b"; "c"; "a"; "d"; "e"]
    (Problems.compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])


let suite =
  "Problems Tests" >::: [
    "test_last" >:: test_last;
    "test_last_two" >:: test_last_two;
    "test_at" >:: test_at;
    "test_length" >:: test_length;
    "test_rev" >:: test_rev;
    "test_is_palindrome" >:: test_is_palindrome;
    "test_flatten" >:: test_flatten;
    "test_compress" >:: test_compress;
  ]

let () =
  run_test_tt_main suite
