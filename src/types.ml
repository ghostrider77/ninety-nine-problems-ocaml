type 'a node =
  | One of 'a
  | Many of 'a node list


type 'a rle =
  | One of 'a
  | Many of int * 'a


type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr


type cell = {x : int; y : int}


module CellMap = Map.Make(
  struct
    type t = cell
    let compare = compare
  end)


module CharSet = Set.Make(Char)


module CodeTreeSet = Set.Make(
  struct
    type t = Code_tree.t
    let compare = Code_tree.compare
  end)
module StringMap = Map.Make(String)
