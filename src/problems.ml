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


let split xs n =
  let rec aux acc rest k =
    if k <= 0 then (List.rev acc, rest)
    else
      match rest with
        | [] -> aux acc rest 0
        | h :: tl -> aux (h :: acc) tl (k - 1) in
  aux [] xs n


let slice xs i k =
  List.(xs |> drop i |> take (k - i + 1))


let rotate xs n =
  let length = List.length xs in
  let k =
    if length = 0 then 0
    else
      let r = n mod length in
      if r >= 0 then r else r + length in
  let first, second = split xs k in
  second @ first


let remove_at k xs =
  List.filteri (fun ix _ -> ix <> k) xs


let insert_at elt n xs =
  let rec aux acc k = function
    | h :: tl when k < n -> aux (h :: acc) (k + 1) tl
    | rest -> List.rev (elt :: acc) @ rest in
  aux [] 0 xs


let range a b =
  let n = min a b in
  let m = max a b in
  let rs = List.init (m - n + 1) (fun k -> n + k) in
  if a <= b then rs else List.rev rs


let rand_select xs k =
  let select_one ys n =
    let y = List.nth ys n in
    (y, remove_at n ys) in
  let rec aux acc ys length n =
    if n = 0 || length = 0 then acc
    else
      let ix = Random.int length in
      let (elt, rest) = select_one ys ix in
      aux (elt :: acc) rest (length - 1) (n - 1) in
  aux [] xs (List.length xs) k


let lotto_select n m =
  let numbers = range 1 m in
  rand_select numbers n


let permutation xs =
  rand_select xs (List.length xs)


let rec extract k xs =
  if k <= 0 then [[]]
  else
    match xs with
      | [] -> []
      | h :: tl ->
          let contain_h = List.map (List.cons h) (extract (k - 1) tl) in
          let skip_h = extract k tl in
          contain_h @ skip_h


let group xs sizes =
  let extend size groups =
    let elements = List.concat groups in
    let xs' = List.filter (fun x -> not (List.mem x elements)) xs in
    let combinations = extract size xs' in
    List.map (fun cs -> cs :: groups) combinations in
  let rec aux acc = function
    | [] -> List.(acc |> map rev)
    | size :: rest ->
      let acc' = List.concat_map (extend size) acc in
      aux acc' rest in
  aux [[]] sizes


let length_sort xs =
  List.(sort compare_lengths xs)


let frequency_sort xs =
  let lengths = List.(map length xs) in
  let frequencies = lengths |> List.sort compare |> encode |> List.map (fun (cnt, len) -> (len, cnt)) in
  let xs_with_length_counts = List.map2 (fun x len -> (x, List.assoc len frequencies)) xs lengths in
  xs_with_length_counts |> List.sort (fun (_, l1) (_, l2) -> compare l1 l2) |> List.map fst
