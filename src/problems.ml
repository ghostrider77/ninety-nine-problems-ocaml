open Types

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl


let rec last_two = function
  | [a; b] -> Some (a, b)
  | _ :: b :: rest -> last_two (b :: rest)
  | _ -> None


let rec at n xs =
  if n < 0 then None
  else match xs with
    | [] -> None
    | h :: tl ->
        if n = 0 then Some h
        else at (n - 1) tl


let length xs =
  let rec aux acc = function
    | [] -> acc
    | _ :: tl -> aux (acc + 1) tl in
  aux 0 xs


let rev xs =
  let rec aux acc = function
    | [] -> acc
    | h :: tl -> aux (h :: acc) tl in
  aux [] xs


let is_palindrome xs =
  List.rev xs = xs


let flatten xs =
  let rec aux acc = function
    | [] -> acc
    | h :: tl ->
        match h with
          | One elt -> aux (elt :: acc) tl
          | Many elts -> aux (aux acc elts) tl in
  List.rev (aux [] xs)


let compress = function
  | [] -> []
  | x :: xs ->
      let rec aux acc current_elt = function
        | [] -> List.rev acc
        | h :: tl ->
            if h = current_elt then aux acc current_elt tl
            else aux (h :: acc) h tl in
      aux [x] x xs


let pack = function
  | [] -> []
  | x :: xs ->
      let rec aux acc elts current_elt = function
        | [] -> List.rev (elts :: acc)
        | h :: tl ->
            if h = current_elt then aux acc (h :: elts) current_elt tl
            else aux (elts :: acc) [h] h tl in
      aux [] [x] x xs
