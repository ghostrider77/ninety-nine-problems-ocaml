open Types

module CodeTreeSet = Set.Make(
  struct
    type t = Code_tree.t
    let compare = Code_tree.compare
  end)
module StringMap = Map.Make(String)


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


let is_prime n =
  if n = 2 then true
  else if n = 1 || n mod 2 = 0 then false
  else
    let limit = n |> float |> sqrt |> int_of_float in
    let rec aux k =
      if k > limit then true
      else if n mod k = 0 then false
      else aux (k + 2) in
    aux 3


let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)


let coprime a b =
  gcd a b = 1


let phi n =
  Seq.(ints 1 |> take n |> fold_left (fun acc k -> if coprime n k then acc + 1 else acc) 0)


let factors n =
  let rec aux acc k p =
    if p > k then List.rev acc
    else if k mod p = 0 then aux (p :: acc) (k / p) p
    else aux acc k (if p = 2 then 3 else p + 2) in
  aux [] n 2


let factors2 n =
  let rec aux acc k p =
    if p > k then List.rev acc
    else if k mod p <> 0 then aux acc k (if p = 2 then 3 else p + 2)
    else
      let k' = k / p in
      match acc with
        | ((prime, m) :: rest) when prime = p -> aux ((prime, m + 1) :: rest) k' p
        | _ -> aux ((p, 1) :: acc) k' p in
  aux [] n 2


let phi_improved n =
  let factorization = factors2 n in
  let pow m k =
    let rec aux acc alpha =
      if alpha = k then acc
      else aux (acc * m) (alpha + 1) in
    aux 1 0 in
  List.fold_left (fun acc (p, k) -> acc * (p - 1) * pow p (k - 1)) 1 factorization


let timeit func x =
  let t = Unix.gettimeofday () in
  ignore (func x);
  let t' = Unix.gettimeofday () in
  (t' -. t)


let all_primes a b =
  let rec aux acc n =
    if n > b then List.rev acc
    else if is_prime n then aux (n :: acc) (n + 1)
    else aux acc (n + 1) in
  aux [] a


let goldbach n =
  let rec aux k =
    if is_prime k && is_prime (n - k) then (k, n - k)
    else aux (k + 1) in
  aux 2


let goldbach_list a b =
  let rec aux acc k =
    if k > b then List.rev acc
    else if k mod 2 = 1 then aux acc (k + 1)
    else aux ((k, goldbach k) :: acc) (k + 2) in
  aux [] a


let table2 a b expression =
  let rec evaluate a_value b_value = function
    | Var x ->
        if x = a then a_value
        else if x = b then b_value
        else failwith "Unknown variable name"
    | Not expr -> not (evaluate a_value b_value expr)
    | And (expr1, expr2) -> (evaluate a_value b_value expr1) && (evaluate a_value b_value expr2)
    | Or (expr1, expr2) -> (evaluate a_value b_value expr1) || (evaluate a_value b_value expr2) in
  let values = [(true, true); (true, false); (false, true); (false, false)] in
  List.map (fun (a_val, b_val) -> (a_val, b_val, evaluate a_val b_val expression)) values


let table variable_names expression =
  let product xs n =
    let rec aux acc k =
      if k = n then acc
      else
        let acc' = List.concat_map (fun x -> List.map (List.cons x) acc) xs in
        aux acc' (k + 1) in
    aux [[]] 0 in
  let rec evaluate values = function
    | Var x -> StringMap.find x values
    | Not expr -> not (evaluate values expr)
    | And (expr1, expr2) -> (evaluate values expr1) && (evaluate values expr2)
    | Or (expr1, expr2) -> (evaluate values expr1) || (evaluate values expr2) in
  let process bool_values =
    let assoc = List.combine variable_names bool_values in
    let values = StringMap.of_list assoc in
    let result = evaluate values expression in
    (assoc, result) in
  let n = List.length variable_names in
  let all_inputs = product [true; false] n in
  List.map process all_inputs


let gray n =
  let bitstring_of_int m =
    let rec aux acc k digit =
      if digit = n then acc |> List.map string_of_int |> String.concat ""
      else aux ((k mod 2) :: acc) (k / 2) (digit + 1) in
    aux [] m 0 in
  let encode k =
    k lxor (k lsr 1) in
  let pow m k =
    let rec aux acc alpha =
      if alpha = k then acc
      else aux (acc * m) (alpha + 1) in
    aux 1 0 in
  let size = pow 2 n in
  List.map (Fun.compose bitstring_of_int encode) @@ List.init size Fun.id


let huffman frequencies =
  let open Code_tree in
  let encode fs =
    let rec aux queue =
      if CodeTreeSet.cardinal queue <= 1 then CodeTreeSet.min_elt queue
      else
        let t1 = CodeTreeSet.min_elt queue in
        let queue' = CodeTreeSet.remove t1 queue in
        let t2 = CodeTreeSet.min_elt queue' in
        let queue'' = CodeTreeSet.remove t2 queue' in
        aux (CodeTreeSet.add (merge t1 t2) queue'') in
    aux (List.fold_left (fun acc (name, weight) -> CodeTreeSet.add (Leaf {name; weight}) acc) CodeTreeSet.empty fs) in
  let tree = encode frequencies in
  traverse tree


let cbal_tree n =
  let open Binary_tree in
  let make_nodes left_subtrees right_subtrees =
    List.concat_map (fun l -> List.map (fun r -> Node ('x', l, r)) right_subtrees) left_subtrees in
  let rec balanced_trees k =
    if k = 0 then [Empty]
    else if k mod 2 = 1 then
      let ts = balanced_trees (k / 2) in
      make_nodes ts ts
    else
      let ts1 = balanced_trees (k / 2) in
      let ts2 = balanced_trees ((k - 1) / 2) in
      make_nodes ts1 ts2 @ make_nodes ts2 ts1 in
  balanced_trees n


let is_symmetric tree =
  let open Binary_tree in
  let rec is_mirror t1 t2 =
    match (t1, t2) with
      | (Empty, Empty) -> true
      | (Node(_, l1, r1), Node(_, l2, r2)) -> is_mirror l1 r2 && is_mirror r1 l2
      | _ -> false in
  match tree with
    | Empty -> true
    | Node (_, l, r) -> is_mirror l r


let construct xs =
  let open Binary_tree in
  let rec insert x = function
    | Empty -> Node (x, Empty, Empty)
    | Node (v, l, r) as tree ->
        if x < v then Node (v, insert x l, r)
        else if x = v then tree
        else Node (v, l, insert x r) in
  List.fold_left (Fun.flip insert) Empty xs


let sym_cbal_trees n =
  List.filter is_symmetric (cbal_tree n)
