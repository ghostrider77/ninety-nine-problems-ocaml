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
