type t =
  | Leaf of {name : string; weight : int}
  | Node of {left : t; right: t; names : string list; weight : int}


let compare t1 t2 =
  match (t1, t2) with
    | (Leaf {name = n1; weight = w1}, Leaf {name = n2; weight = w2}) -> compare (w1, n1) (w2, n2)
    | (Leaf {name; weight = w1}, Node {names; weight = w2; _}) -> compare (w1, [name]) (w2, names)
    | (Node {names; weight = w1; _}, Leaf {name; weight = w2}) -> compare (w1, names) (w2, [name])
    | (Node {names = ns1; weight = w1; _}, Node {names = ns2; weight = w2; _}) -> compare (w1, ns1) (w2, ns2)


let weight = function
  | Leaf {weight; _} -> weight
  | Node {weight; _} -> weight


let names = function
  | Leaf {name; _} -> [name]
  | Node {names; _} -> names


let merge t1 t2 =
  let w = weight t1 + weight t2 in
  if compare t1 t2 <= 0 then Node {left = t1; right = t2; names = names t1 @ names t2; weight = w}
  else Node {left = t2; right = t1; names = names t2 @ names t1; weight = w}


let rec traverse = function
  | Leaf {name; _} -> [(name, "")]
  | Node {left; right; _} ->
      let ls = List.map (fun (name, s) -> (name, "0" ^ s)) @@ traverse left in
      let rs = List.map (fun (name, s) -> (name, "1" ^ s)) @@ traverse right in
      ls @ rs
