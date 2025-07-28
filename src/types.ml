type 'a node =
  | One of 'a
  | Many of 'a node list


type 'a rle =
  | One of 'a
  | Many of int * 'a
