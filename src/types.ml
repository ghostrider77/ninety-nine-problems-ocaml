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
