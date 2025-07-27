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
