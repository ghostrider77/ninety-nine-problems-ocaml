type 'a t =
  | Empty
  | Node of 'a * 'a t * 'a t
