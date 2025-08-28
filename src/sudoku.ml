open Types


module Solver : sig
  type t

  val of_string : string -> t
  (** Converts the string representation of a puzzle to a Sudoku puzzle. *)

  val to_string : t -> string
  (** Converts a Sudoku puzzle to its string representation. *)

  val solve : t -> t option
  (** Solves a Sudoku puzzle when possible. *)
end = struct
  type t = char CellMap.t

  let board_size = 9
  let square_size = 3
  let valid_values = CharSet.of_list ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']


  let get_values_in_square board {x; y} =
    let x0 = x / square_size in
    let y0 = y / square_size in
    let xs = Seq.init square_size (fun i -> square_size * x0 + i) in
    let ys = Seq.init square_size (fun j -> square_size * y0 + j) in
    let values = Seq.concat_map (fun i -> Seq.map (fun j -> CellMap.find {x = i; y = j} board) ys) xs in
    CharSet.of_seq values


  let get_empty_cells board =
    let possible_values ({x = x0; y = y0} as cell) =
      if CellMap.find cell board <> '.' then []
      else
        let xs = Seq.init board_size Fun.id in
        let row = CharSet.of_seq @@ Seq.map (fun y -> CellMap.find {x = x0; y} board) xs in
        let col = CharSet.of_seq @@ Seq.map (fun x -> CellMap.find {x; y = y0} board) xs in
        let square = get_values_in_square board cell in
        CharSet.elements (CharSet.diff valid_values (CharSet.union (CharSet.union row col) square)) in
    board
      |> CellMap.filter (fun _ v -> v = '.')
      |> CellMap.bindings
      |> List.map (fun (cell, _) -> (cell, possible_values cell))
      |> List.sort (fun (_, vs1) (_, vs2) -> List.compare_lengths vs1 vs2)


  let of_string s =
    let rows = s |> String.split_on_char '\n' |> List.filteri (fun ix _ -> (ix + 1) mod 4 <> 0) in
    let filter row =
      row |> String.to_seq |> Seq.filter (fun c -> c <> '|' && c <> ' ') |> List.of_seq in
    let cells = List.concat @@ List.mapi (fun x row -> List.mapi (fun y c -> ({x; y}, c)) @@ filter row) rows in
    CellMap.of_list cells


  let to_string board =
    let get_char x y = CellMap.find {x; y} board in
    let separator_line =
      let first = String.make (3 * square_size - 1) '-' in
      let middle = String.make (3 * square_size) '-' in
      Printf.sprintf "%s+%s+%s" first middle first in
    let string_of_row ix =
      let first = Printf.sprintf "%c  %c  %c " (get_char ix 0) (get_char ix 1) (get_char ix 2) in
      let second = Printf.sprintf " %c  %c  %c " (get_char ix 3) (get_char ix 4) (get_char ix 5) in
      let third = Printf.sprintf " %c  %c  %c" (get_char ix 6) (get_char ix 7) (get_char ix 8) in
      Printf.sprintf "%s|%s|%s" first second third in
    let rows = [
        string_of_row 0
      ; string_of_row 1
      ; string_of_row 2
      ; separator_line
      ; string_of_row 3
      ; string_of_row 4
      ; string_of_row 5
      ; separator_line
      ; string_of_row 6
      ; string_of_row 7
      ; string_of_row 8
      ] in
    String.concat "\n" rows


  let solve initial_board =
    let rec find_solution board =
      match get_empty_cells board with
        | [] -> [board]
        | (_, []) :: _ -> []
        | (cell, possible_values) :: _ ->
            List.concat_map (fun value -> find_solution (CellMap.add cell value board)) possible_values in
    match find_solution initial_board with
      | [] -> None
      | h :: _ -> Some h

end
