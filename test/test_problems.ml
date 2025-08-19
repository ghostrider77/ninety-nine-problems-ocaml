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


let test_group _ =
  assert_equal [[]] (Problems.group [] []);
  assert_equal [[['a']; ['b']]; [['b']; ['a']]] (Problems.group ['a'; 'b'] [1; 1]);
  assert_equal [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]]] (Problems.group ["a"; "b"; "c"] [2; 1])


let test_sorting_by_length _ =
  assert_equal [[]; [3]; [1; 2]; [1; 2; 3]] (Problems.length_sort [[1; 2; 3]; []; [1; 2]; [3]]);
  assert_equal [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["i"; "j"; "k"; "l"]]
    (Problems.length_sort
      [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]])


let test_sorting_by_length_frequency _ =
  assert_equal [[]; [3]; [1; 2]; [1; 2; 3]] (Problems.frequency_sort [[]; [3]; [1; 2]; [1; 2; 3]]);
  assert_equal [[1; 2; 3]; [1; 2]; [2; 3]; [1]; [2]; [3]]
    (Problems.frequency_sort [[1; 2]; [1]; [2]; [3]; [1; 2; 3]; [2; 3]])


let test_is_prime _ =
  assert_bool "Not a prime" (Problems.is_prime 2);
  assert_bool "Not a prime" (Problems.is_prime 3);
  assert_bool "Not a prime" (Problems.is_prime 5);
  assert_bool "Not a prime" (Problems.is_prime 7);
  assert_bool "Not a prime" (Problems.is_prime 11);
  assert_bool "Not a prime" (Problems.is_prime 101);
  assert_bool "Not a prime" (Problems.is_prime 103);
  assert_bool "It's a prime" (not @@ Problems.is_prime 4);
  assert_bool "It's a prime" (not @@ Problems.is_prime 6);
  assert_bool "It's a prime" (not @@ Problems.is_prime 21);
  assert_bool "It's a prime" (not @@ Problems.is_prime 117)


let test_gcd _ =
  assert_equal 1 (Problems.gcd 13 27);
  assert_equal 2 (Problems.gcd 20536 7826);
  assert_equal 6 (Problems.gcd 30 12)


let test_coprime _ =
  assert_bool "Should be coprime." (Problems.coprime 13 27);
  assert_bool "Should not be coprime." (not @@ Problems.coprime 20536 7826)


let test_naive_phi _ =
  assert_equal 4 (Problems.phi 10);
  assert_equal 2 (Problems.phi 6);
  assert_equal 6 (Problems.phi 7)


let test_factors _ =
  assert_equal [] (Problems.factors 1);
  assert_equal [7] (Problems.factors 7);
  assert_equal [2; 2; 3] (Problems.factors 12);
  assert_equal [2; 7; 11; 11; 101; 103] (Problems.factors 17622682)


let test_prime_factorization _ =
  assert_equal [] (Problems.factors 1);
  assert_equal [(7, 1)] (Problems.factors2 7);
  assert_equal [(2, 2); (3, 1)] (Problems.factors2 12);
  assert_equal [(2, 1); (7, 1); (11, 2); (101, 1); (103, 1)] (Problems.factors2 17622682)


let test_improved_phi _ =
  assert_equal 4 (Problems.phi_improved 10);
  assert_equal 2 (Problems.phi_improved 6);
  assert_equal 6 (Problems.phi_improved 7)


let test_that_improved_phi_is_faster _ =
  let n = 10090 in
  let t1 = Problems.timeit Problems.phi n in
  let t2 = Problems.timeit Problems.phi_improved n in
  assert_bool "Should calculate phi faster" (100.0 *. t2 < t1)


let test_that_primes_in_range_are_calculated _ =
  assert_equal [2; 3; 5; 7; 11] (Problems.all_primes 1 12);
  assert_equal [] (Problems.all_primes 20 22);
  assert_equal 1000 (List.length (Problems.all_primes 2 7920))


let test_goldbach _ =
  assert_equal (3, 5) (Problems.goldbach 8);
  assert_equal (5, 23) (Problems.goldbach 28)


let test_goldbach_list _ =
  assert_equal [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13)); (20, (3, 17))]
    (Problems.goldbach_list 9 20)


let test_logical_tables_with_two_variables _ =
  assert_equal [(true, true, true); (true, false, true); (false, true, true); (false, false, true)]
    (Problems.table2 "a" "b" (Or (Var "a", Not (Var "a"))));
  assert_equal [(true, true, true); (true, false, true); (false, true, false); (false, false, false)]
    (Problems.table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))))


let test_logical_tables _ =
  assert_equal [([("a", true)], false); ([("a", false)], false)] (Problems.table ["a"] (And (Var "a", Not (Var "a"))));
  assert_equal
    [ ([("a", true); ("b", true)], true)
    ; ([("a", true); ("b", false)], true)
    ; ([("a", false); ("b", true)], false)
    ; ([("a", false); ("b", false)], false)
    ]
    (Problems.table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b"))))


let test_gray_code _ =
  assert_equal ["0"; "1"] (Problems.gray 1);
  assert_equal ["00"; "01"; "11"; "10"] (Problems.gray 2);
  assert_equal ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] (Problems.gray 3)


let test_huffman_encoding _ =
  let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)] in
  let expected = [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")] in
  assert_equal expected (Problems.huffman fs)


let test_balanced_binary_trees _ =
  let open Binary_tree in
  assert_equal [Empty] (Problems.cbal_tree 0);
  assert_equal [Node ('x', Empty, Empty)] (Problems.cbal_tree 1);
  assert_equal [Node ('x', Node ('x', Empty, Empty), Empty); Node ('x', Empty, Node ('x', Empty, Empty))]
    (Problems.cbal_tree 2)


let test_binary_tree_symmetry _ =
  let open Binary_tree in
  assert_equal true (Problems.is_symmetric Empty);
  assert_equal true (Problems.is_symmetric (Node ('x', Empty, Empty)));
  assert_equal false (Problems.is_symmetric (Node ('x', Node ('y', Empty, Empty), Empty)))


let test_binary_search_tree _ =
  let open Binary_tree in
  assert_equal Empty (Problems.construct []);
  assert_equal (Node (1, Empty, Empty)) (Problems.construct [1]);
  assert_equal (Node (1, Empty, Node (2, Empty, Node (3, Empty, Empty)))) (Problems.construct [1; 2; 3]);
  assert_equal (Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))) (Problems.construct [2; 1; 3])


let test_symmetric_balanced_binary_trees _ =
  let open Binary_tree in
  assert_equal [Empty] (Problems.sym_cbal_trees 0);
  assert_equal [Node ('x', Empty, Empty)] (Problems.sym_cbal_trees 1);
  assert_equal [] (Problems.sym_cbal_trees 2)


let test_height_balanced_binary_trees _ =
  let open Binary_tree in
  assert_equal [Empty] (Problems.hbal_tree 0);
  assert_equal [Node ('x', Empty, Empty)] (Problems.hbal_tree 1);
  assert_equal
    [ Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
    ; Node ('x', Node('x', Empty, Empty), Empty)
    ; Node ('x', Empty, Node('x', Empty, Empty))
    ]
    (Problems.hbal_tree 2)


let test_count_leaves _ =
  let open Binary_tree in
  assert_equal 0 (Problems.count_leaves Empty);
  assert_equal 1 (Problems.count_leaves @@ Node ('x', Empty, Empty));
  assert_equal 2
    (Problems.count_leaves @@ Node ('x', Empty, Node ('y', Node ('z', Empty, Empty), Node ('w', Empty, Empty))))


let test_leaves _ =
  let open Binary_tree in
  assert_equal [] (Problems.leaves Empty);
  assert_equal ['z'; 'w']
    (Problems.leaves @@ Node ('x', Empty, Node ('y', Node ('z', Empty, Empty), Node ('w', Empty, Empty))))


let test_internals _ =
  let open Binary_tree in
  assert_equal [] (Problems.leaves Empty);
  assert_equal [] (Problems.internals @@ Node ('x', Empty, Empty));
  assert_equal ['x'; 'y']
    (Problems.internals @@ Node ('x', Empty, Node ('y', Node ('z', Empty, Empty), Node ('w', Empty, Empty))))


let test_collecting_nodes_at_given_level _ =
  let open Binary_tree in
  let tree = Node ('x', Empty, Node ('y', Node ('z', Empty, Empty), Node ('w', Empty, Empty))) in
  assert_equal [] (Problems.at_level Empty 2);
  assert_equal ['x'] (Problems.at_level tree 1);
  assert_equal ['y'] (Problems.at_level tree 2);
  assert_equal ['z'; 'w'] (Problems.at_level tree 3);
  assert_equal [] (Problems.at_level tree 4)


let test_complete_binary_tree _ =
  let open Binary_tree in
  assert_equal Empty (Problems.complete_binary_tree []);
  assert_equal (Node ('x', Empty, Empty)) (Problems.complete_binary_tree ['x']);
  assert_equal (Node ('a', Node ('b', Empty, Empty), Empty)) (Problems.complete_binary_tree ['a'; 'b'])

let test_layout_binary_tree_1 _ =
  let open Binary_tree in
  let tree = Node ("x", Node ("y", Empty, Node ("a", Empty, Empty)), Node ("z", Empty, Empty)) in
  let expected = Node (("x", (3, 1)),
    Node (("y", (1, 2)), Empty, Node (("a", (2, 3)), Empty, Empty)),
    Node (("z", (4, 2)), Empty, Empty)) in
  assert_equal Empty (Problems.layout_binary_tree_1 Empty);
  assert_equal expected (Problems.layout_binary_tree_1 tree)


let test_queens_positions _ =
  assert_equal [[1]] (Problems.queens_positions 1);
  assert_equal [] (Problems.queens_positions 2);
  assert_equal [] (Problems.queens_positions 3);
  assert_equal [[3; 1; 4; 2]; [2; 4; 1; 3]] (Problems.queens_positions 4)


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
    "test_group" >:: test_group;
    "test_length_sort" >:: test_sorting_by_length;
    "test_frequency_sort" >:: test_sorting_by_length_frequency;
    "test_if_a_number_is_prime" >:: test_is_prime;
    "test_gcd" >:: test_gcd;
    "test_coprime" >:: test_coprime;
    "test_naive_calculation_of_phi" >:: test_naive_phi;
    "test_factors" >:: test_factors;
    "test_prime_factorization" >:: test_prime_factorization;
    "test_improved_phi" >:: test_improved_phi;
    "test_phi_elapsed_time" >:: test_that_improved_phi_is_faster;
    "test_primes_in_range" >:: test_that_primes_in_range_are_calculated;
    "test_goldbach" >:: test_goldbach;
    "test_goldbach_list" >:: test_goldbach_list;
    "test_logical_tables_with_two_variables" >:: test_logical_tables_with_two_variables;
    "test_logical_tables" >:: test_logical_tables;
    "test_gray_code" >:: test_gray_code;
    "test_huffman_encoding" >:: test_huffman_encoding;
    "test_balanced_binary_trees" >:: test_balanced_binary_trees;
    "test_symmetric_binary_trees" >:: test_binary_tree_symmetry;
    "test_binary_search_tree_construction" >:: test_binary_search_tree;
    "test_symmetric_balanced_binary_trees" >:: test_symmetric_balanced_binary_trees;
    "test_height_balanced_binary_trees" >:: test_height_balanced_binary_trees;
    "test_count_leaves" >:: test_count_leaves;
    "test_leaves" >:: test_leaves;
    "test_internals" >:: test_internals;
    "test_collecting_nodes_at_given_level" >:: test_collecting_nodes_at_given_level;
    "test_complete_binary_tree" >:: test_complete_binary_tree;
    "test_queens_positions" >:: test_queens_positions;
    "test_binary_tree_layout_1" >:: test_layout_binary_tree_1;
  ]


let () =
  run_test_tt_main suite
