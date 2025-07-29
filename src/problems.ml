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
  let rec aux (acc : 'a list) (ys : 'a node list) : 'a list =
    match ys with
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


let encode xs =
  xs
    |> pack
    |> List.map (fun ds -> (List.length ds, List.hd ds))


let encode2 xs =
  let process_duplicates = function
    | [] -> failwith "Duplicate sublist cannot be empty."
    | (d :: _) as ds ->
        let n = List.length ds in
        if n = 1 then One d
        else Many (n, d) in
  xs
    |> pack
    |> List.map process_duplicates


let decode ds =
  let rec aux acc = function
    | [] -> List.rev acc
    | One elt :: tl -> aux (elt :: acc) tl
    | Many (2, elt) :: tl -> aux (elt :: acc) (One elt :: tl)
    | Many (n, elt) :: tl when n > 2 -> aux (elt :: acc) (Many (n - 1, elt) :: tl)
    | _ -> failwith "Cannot decode wrongly RLE-encoded list." in
  aux [] ds


let encode3 = function
  | [] -> []
  | x :: xs ->
      let rec aux (acc : 'a rle list) ((cnt, elt) : int * 'a) = function
        | [] -> List.rev ((if cnt = 1 then One elt else Many (cnt, elt)) :: acc)
        | h :: tl ->
            if h = elt then aux acc (cnt + 1, elt) tl
            else if cnt = 1 then aux (One elt :: acc) (1, h) tl
            else aux ((Many (cnt, elt)) :: acc) (1, h) tl in
      aux [] (1, x) xs


let duplicate xs =
  List.concat_map (fun x -> [x; x]) xs


let replicate xs n =
  List.concat_map (fun x -> List.init n (Fun.const x)) xs


let drop xs n =
  List.filteri (fun ix _ -> (ix + 1) mod n <> 0) xs
