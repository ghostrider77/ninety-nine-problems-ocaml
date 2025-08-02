open OUnit2

open Ninety_nine_ocaml
open Ninety_nine_ocaml.Types


let is_random_sample xs sample =
  let count_occurrences xs =
    let table = Hashtbl.create 100 in
    List.iter
      (fun x -> let cnt =  Option.value (Hashtbl.find_opt table x) ~default:0 in Hashtbl.replace table x (cnt + 1)) xs;
    table in
  let n = List.length xs in
  let k = List.length sample in
  let counts1 = count_occurrences xs in
  let counts2 = count_occurrences sample in
  k <= n &&
    Seq.for_all (fun (x, cnt) -> cnt <= Option.value (Hashtbl.find_opt counts1 x) ~default:0) @@ Hashtbl.to_seq counts2


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
  assert_equal ["a"; "b"; "c"; "d"; "e"] (Problems.flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]])


let test_compress _ =
  assert_equal [] (Problems.compress []);
  assert_equal [1; 2; 3] (Problems.compress [1; 2; 3]);
  assert_equal ['a'; 'b'; 'c'] (Problems.compress ['a'; 'b'; 'b'; 'b'; 'c']);
  assert_equal ["a"; "b"; "c"; "a"; "d"; "e"]
    (Problems.compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])


let test_pack _ =
  assert_equal [] (Problems.pack []);
  assert_equal [[1]; [2]; [3]] (Problems.pack [1; 2; 3]);
  assert_equal [[1; 1; 1]] (Problems.pack [1; 1; 1]);
  assert_equal [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
    (Problems.pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"])


let test_encode _ =
  assert_equal [] (Problems.encode []);
  assert_equal [(1, 'a'); (1, 'b'); (1, 'c')] (Problems.encode ['a'; 'b'; 'c']);
  assert_equal [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
    (Problems.encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])


let test_encode2 _ =
  assert_equal [] (Problems.encode2 []);
  assert_equal [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
    (Problems.encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])


let test_decode _ =
  assert_equal [] (Problems.decode []);
  assert_equal ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    (Problems.decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])


let test_encode3 _ =
  assert_equal [] (Problems.encode3 []);
  assert_equal [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
    (Problems.encode3 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])


let test_duplicate _ =
  assert_equal [] (Problems.duplicate []);
  assert_equal [1; 1; 2; 2; 3; 3] (Problems.duplicate [1; 2; 3])


let test_replicate _ =
  assert_equal [] (Problems.replicate [] 10);
  assert_equal ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] (Problems.replicate ["a"; "b"; "c"] 3)


let test_drop _ =
  assert_equal [] (Problems.drop [] 2);
  assert_equal [1] (Problems.drop [1; 2] 2);
  assert_equal ["a"; "b"; "d"; "e"; "g"; "h"; "j"] (Problems.drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3)


let test_split _ =
  assert_equal ([], [1; 2; 3]) (Problems.split [1; 2; 3] 0);
  assert_equal ([1; 2], [3]) (Problems.split [1; 2; 3] 2);
  assert_equal ([1; 2; 3], []) (Problems.split [1; 2; 3] 4)


let test_slice _ =
  assert_equal [1] (Problems.slice [1; 2; 3] 0 0);
  assert_equal ["c"; "d"; "e"; "f"; "g"] (Problems.slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6)


let test_rotate _ =
  assert_equal [] (Problems.rotate [] 0);
  assert_equal [] (Problems.rotate [] 10);
  assert_equal [1; 2; 3] (Problems.rotate [1; 2; 3] 0);
  assert_equal [2; 3; 1] (Problems.rotate [1; 2; 3] 1);
  assert_equal [2; 3; 1] (Problems.rotate [1; 2; 3] 10);
  assert_equal [3; 1; 2] (Problems.rotate [1; 2; 3] (-1));
  assert_equal ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] (Problems.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3)


let test_remove_at _ =
  assert_equal ["a"; "c"; "d"] (Problems.remove_at 1 ["a"; "b"; "c"; "d"])


let test_insert_at _ =
  assert_equal ['a'] (Problems.insert_at 'a' 0 []);
  assert_equal ['a'] (Problems.insert_at 'a' 10 []);
  assert_equal [100; 1; 2; 3] (Problems.insert_at 100 0 [1; 2; 3]);
  assert_equal [1; 2; 100; 3] (Problems.insert_at 100 2 [1; 2; 3]);
  assert_equal [1; 2; 3; 100] (Problems.insert_at 100 5 [1; 2; 3])


let test_range _ =
  assert_equal [1] (Problems.range 1 1);
  assert_equal [4; 5; 6; 7; 8; 9] (Problems.range 4 9);
  assert_equal [9; 8; 7; 6; 5; 4] (Problems.range 9 4)


let test_rand_select _ =
  assert_equal [] (Problems.rand_select [1; 2; 3] 0);
  assert_bool "Not a random sample" (is_random_sample [1; 2; 3] (Problems.rand_select [1; 2; 3] 2))


let test_lotto_select _ =
  let m = 90 in
  let ns = List.init m (fun ix -> ix + 1) in
  assert_bool "Not a random sample" (is_random_sample ns (Problems.lotto_select 5 m))


let test_permutation _ =
  let is_permutation xs ps =
    List.sort compare xs = List.sort compare ps in
  let xs = ["a"; "b"; "c"; "d"; "e"; "f"] in
  assert_bool "Not a permutation" (is_permutation xs (Problems.permutation xs))


let test_extract _ =
  assert_equal [[]] (Problems.extract 0 []);
  assert_equal [] (Problems.extract 3 [1; 2]);
  assert_equal
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
    (Problems.extract 2 ["a"; "b"; "c"; "d"])


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
    "test_pack" >:: test_pack;
    "test_encode" >:: test_encode;
    "test_encode2" >:: test_encode2;
    "test_decode" >:: test_decode;
    "test_encode3" >:: test_encode3;
    "test_duplicate" >:: test_duplicate;
    "test_replicate" >:: test_replicate;
    "test_drop" >:: test_drop;
    "test_split" >:: test_split;
    "test_slice" >:: test_slice;
    "test_rotate" >:: test_rotate;
    "test_remove_at" >:: test_remove_at;
    "test_insert_at" >:: test_insert_at;
    "test_range" >:: test_range;
    "test_rand_select" >:: test_rand_select;
    "test_lotto_select" >:: test_lotto_select;
    "test_permutation" >:: test_permutation;
    "test_extract_all_combinations" >:: test_extract;
  ]


let () =
  run_test_tt_main suite
