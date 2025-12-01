let file = "inputs/day1.in"
let string_to_char_list s = s |> String.to_seq |> List.of_seq
let char_list_to_int = List.fold_left (fun acc c -> acc * 10 + Char.code c - Char.code '0') 0

let process_line (pointer : int) (zeros : int) (line : char list) : (int * int) =
  match line with
  | (dir :: rest) ->
    (let number = char_list_to_int rest in
      let pointer =
        match dir with
        | 'R' -> Advent.modulo (pointer + number) 100
        | 'L' -> Advent.modulo (pointer - number) 100
        | _ -> failwith "Invalid direction"
      in
      let zeros = if pointer = 0 then zeros + 1 else zeros in
      (pointer, zeros))
  | _ -> failwith "Invalid line"

let part1 () =
  file
  |> Advent.read_lines
  |> List.map string_to_char_list
  |> List.fold_left (fun acc line -> process_line (fst acc) (snd acc) line) (50, 0)
  |> snd
  |> string_of_int
  |> print_endline

let run () =
  part1 ();
  ()
