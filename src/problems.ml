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
