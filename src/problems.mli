open Types

val last : 'a list -> 'a option
(** Returns the last element of a list *)


val last_two : 'a list -> ('a * 'a) option
(** Finds the last two (last and penultimate) elements of a list.*)


val at : int -> 'a list -> 'a option
(** Finds the {i n}th element of a list.*)


val length : 'a list -> int
(** Calculates the length of a list.*)


val rev : 'a list -> 'a list
(** Reverses a list.*)


val is_palindrome : 'a list -> bool
(** Finds out whether a list is a palindrome.*)


val flatten : 'a node list -> 'a list
(** Flattens a nested list structure.*)


val compress : 'a list -> 'a list
(** Eliminates consecutive duplicates of list elements. *)


val pack : 'a list -> 'a list list
(** Packs consecutive duplicates of list elements into sublists. *)


val encode : 'a list -> (int * 'a) list
(** Perform run-length encoding. *)


val encode2 : 'a list -> 'a rle list
(** Perform run-length encoding. Only elements with duplicates are transferred as (count, element) lists. *)


val decode : 'a rle list -> 'a list
(** Given a run-length code list generated as specified in the previous problem, constructs its uncompressed version. *)


val encode3 : 'a list -> 'a rle list
(** Perform run-length encoding without the intermediate list of consecutive duplicates. *)


val duplicate : 'a list -> 'a list
(** Duplicates the elements of a list. *)


val replicate : 'a list -> int -> 'a list
(** Replicates the elements of a list a given number of times. *)


val drop : 'a list -> int -> 'a list
(** Drops every {i n}th element from a list. *)


val split : 'a list -> int -> 'a list * 'a list
(** Splits a list into two parts; the length of the first part is given. *)


val slice : 'a list -> int -> int -> 'a list
(** Given two indices, {i i} and {i k}, the slice is the list containing the elements between the {i i}th
and {i k}th element. *)


val rotate : 'a list -> int -> 'a list
(** Rotate a list {i N} places to the left. *)


val remove_at : int -> 'a list -> 'a list
(** Remove the {i k}th element from a list. *)


val insert_at : 'a -> int -> 'a list -> 'a list
(** Inserts an element at a given position into a list. *)


val range : int -> int -> int list
(** Creates a list containing all integers within a given range *)


val rand_select : 'a list -> int -> 'a list
(** Extracts a given number of randomly selected elements from a list. *)


val lotto_select : int -> int -> int list
(** Draws {i n} different random numbers from the set {i 1..m}. *)


val permutation : 'a list -> 'a list
(** Generates a random permutation of the elements of a list. *)


val extract : int -> 'a list -> 'a list list
(** Generates the combinations of {i k} distinct objects chosen from the {i n} elements of a list. *)


val group : 'a list -> int list -> 'a list list list
(** Groups the elements of a set into disjoint subsets. *)


val length_sort : 'a list list -> 'a list list
(** Sorts a list of lists according to length of sublists. *)


val frequency_sort : 'a list list -> 'a list list
(** Sorts a list of lists according to the length frequencies of the sublists. *)


val is_prime : int -> bool
(** Determines whether a given integer number is prime. *)


val gcd : int -> int -> int
(** Determines the greatest common divisor of two positive integer numbers. *)


val coprime : int -> int -> bool
(** Determines whether two positive integer numbers are coprime. *)


val phi : int -> int
(** Calculates Euler's totient function in a naive way. *)


val factors : int -> int list
(** Constructs a flat list containing the prime factors in ascending order. *)


val factors2 : int -> (int * int) list
(** Constructs a list containing the prime factors and their multiplicity. *)


val phi_improved : int -> int
(** Calculates Euler's totient function more efficiently. *)


val timeit : ('a -> 'b) -> 'a -> float
(** Calculated elapsed time for a function and its argument. *)


val all_primes : int -> int -> int list
(** Given a range of integers by its lower and upper limit, constructs a list of all prime numbers in that range. *)


val goldbach : int -> int * int
(** Finds the two prime numbers that sum up to a given even integer. *)


val goldbach_list : int -> int -> (int * (int * int)) list
(** Given a range of integers finds a list of all even numbers and their Goldbach composition. *)


val table2 : string -> string -> bool_expr -> (bool * bool * bool) list
(** Returns the truth table of a given logical expression in two variables. *)


val table : string list -> bool_expr -> ((string * bool) list * bool) list
(** Returns the truth table of a given logical expression for any number of variables *)


val gray : int -> string list
(** An {i n}-bit Gray code construction *)


val huffman : (string * int) list -> (string * string) list
(** Constructs the Huffman code for all symbols. *)


val cbal_tree : int -> char Binary_tree.t list
(** Constructs completely balanced binary trees. *)


val is_symmetric : 'a Binary_tree.t -> bool
(** Checks whether a given binary tree is symmetric. *)


val construct : int list -> int Binary_tree.t
(** Constructs a binary search tree from a list of integer numbers. *)


val sym_cbal_trees : int -> char Binary_tree.t list
(** Constructs all symmetric, completely balanced binary trees with a given number of nodes. *)


val hbal_tree : int -> char Binary_tree.t list
(** Constructs height-balanced binary trees. *)


(* val hbal_tree_nodes : int -> char Binary_tree.t list *)
(** Constructs height-balanced binary trees with a given number of nodes. *)


val count_leaves : 'a Binary_tree.t -> int
(** Counts the leaves of a binary tree *)


val leaves : 'a Binary_tree.t -> 'a list
(** Collects the leaves of a binary tree in a list *)


val internals : 'a Binary_tree.t -> 'a list
(** Collects the internal nodes of a binary tree in a list. *)


val at_level : 'a Binary_tree.t -> int -> 'a list
(** Collects the nodes at a given level in a list. *)


val complete_binary_tree : 'a list -> 'a Binary_tree.t
(** Construct a complete binary tree. *)


val layout_binary_tree_1 : 'a Binary_tree.t -> ('a * (int * int)) Binary_tree.t
(** In this layout strategy, the position of a node {i v} is obtained by the following two rules:
    - {i x(v)} is equal to the position of the node {i v} in the inorder sequence;
    - {i y(v)} is equal to the depth of the node {i v} in the tree. *)


(* val layout_binary_tree_2 : 'a Binary_tree.t -> ('a * (int * int)) Binary_tree.t *)
(** In this layout strategy, the position of a node {i v} is obtained by the following two rules:
    - On a given level, the horizontal distance between neighbouring nodes is constant;
    - {i y(v)} is still equal to the depth of the node {i v} in the tree *)


val string_of_tree : char Binary_tree.t -> string
(** Generates string representation of a tree. *)


(* val tree_of_string : string -> char Binary_tree.t *)
(** Generates tree from its string representation. *)


val queens_positions : int -> int list list
(** Places {i n} queens on a chessboard so that no two queens are attacking each other. *)


val sudoku : string -> string
(** Solves a sudoku puzzle.
    The input is expected to be a string like this one:
    {v
    .  .  4 | 8  .  . | .  1  7
    6  7  . | 9  .  . | .  .  .
    5  .  8 | .  3  . | .  .  4
    --------+---------+--------
    3  .  . | 7  4  . | 1  .  .
    .  6  9 | .  .  . | 7  8  .
    .  .  1 | .  6  9 | .  .  5
    --------+---------+--------
    1  .  . | .  8  . | 3  .  6
    .  .  . | .  .  6 | .  9  1
    2  4  . | .  .  1 | 5  .  . v} *)
