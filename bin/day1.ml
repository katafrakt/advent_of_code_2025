let file = "inputs/day1.in"
let string_to_char_list s = s |> String.to_seq |> List.of_seq
let char_list_to_int = List.fold_left (fun acc c -> acc * 10 + Char.code c - Char.code '0') 0

type mode =
  | FinalPosition
  | WithIntermediate

let process_line (pointer : int) (zeros : int) (line : char list) (mode : mode) : (int * int) =
  match line with
  | (dir :: rest) ->
    (let number = char_list_to_int rest in
      let raw_value =
        match dir with
        | 'R' -> pointer + number
        | 'L' -> pointer - number
        | _ -> failwith "Invalid direction"
      in
      let new_pointer = Advent.modulo raw_value 100 in
      let new_zeros = match (mode, raw_value, new_pointer, pointer) with
        | (FinalPosition, _, 0, _) -> 1
        | (FinalPosition, _, _, _) -> 0
        | (WithIntermediate, 0, _, _) -> 1
        | (WithIntermediate, _, _, _) when raw_value > 0 -> raw_value / 100
        | (WithIntermediate, _, _, 0) -> (abs raw_value) / 100
        | (WithIntermediate, _, _, _) -> (abs raw_value) / 100 + 1 in
      (new_pointer, zeros + new_zeros))
  | _ -> failwith "Invalid line"

let process (mode : mode) : unit =
  file
  |> Advent.read_lines
  |> List.map string_to_char_list
  |> List.fold_left (fun acc line -> process_line (fst acc) (snd acc) line mode) (50, 0)
  |> snd
  |> string_of_int
  |> print_endline

let run () =
  process FinalPosition;
  process WithIntermediate
