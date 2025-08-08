open Types

val last : 'a list -> 'a option
(** Returns the last element of a list *)


val last_two : 'a list -> ('a * 'a) option
(** Finds the last two (last and penultimate) elements of a list.*)


val at : int -> 'a list -> 'a option
(** Finds the nth element of a list.*)


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
(** Drops every nth element from a list. *)


val split : 'a list -> int -> 'a list * 'a list
(** Splits a list into two parts; the length of the first part is given. *)


val slice : 'a list -> int -> int -> 'a list
(** Given two indices, i and k, the slice is the list containing the elements between the ith and kth element. *)


val rotate : 'a list -> int -> 'a list
(** Rotate a list N places to the left. *)


val remove_at : int -> 'a list -> 'a list
(** Remove the kth element from a list. *)


val insert_at : 'a -> int -> 'a list -> 'a list
(** Inserts an element at a given position into a list. *)


val range : int -> int -> int list
(** Creates a list containing all integers within a given range *)


val rand_select : 'a list -> int -> 'a list
(** Extracts a given number of randomly selected elements from a list. *)


val lotto_select : int -> int -> int list
(** Draws n different random numbers from the set 1..m. *)


val permutation : 'a list -> 'a list
(** Generates a random permutation of the elements of a list. *)


val extract : int -> 'a list -> 'a list list
(** Generates the combinations of k distinct objects chosen from the n elements of a list. *)


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
