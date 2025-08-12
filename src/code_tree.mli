type t =
  | Leaf of {name : string; weight : int}
  | Node of {left : t; right: t; names : string list; weight : int}


val compare : t -> t -> int
(** Compares two Huffman trees. *)


val merge : t -> t -> t
(** Merges two Huffman trees. *)


val traverse : t -> (string * string) list
(** Traverses a Huffman tree and collects the encoding of each character. *)
