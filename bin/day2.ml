let file = "inputs/day2.in"
let string_to_char_list s = s |> String.to_seq |> List.of_seq

type mode = Part1 | Part2

let char_list_to_int =
  List.fold_left (fun acc c -> (acc * 10) + Char.code c - Char.code '0') 0

let has_cycle_of_len (len : int) (list : 'a list) : bool =
  let rec check (l1 : 'a list) (l2 : 'a list) =
    let l1len = List.length l1 in
    let l2hd = Advent.ListExt.take l1len l2 in
    if l2hd = [] then true
    else if l1len != List.length l2hd then false
    else if l1 = l2hd then check l1 (Advent.ListExt.drop l1len l2)
    else false
  in
  check (Advent.ListExt.take len list) (Advent.ListExt.drop len list)

let rec has_cycle_of_len_up_to len list =
  if len == 0 then false
  else if has_cycle_of_len len list then true
  else has_cycle_of_len_up_to (len - 1) list

let is_bad_id number mode =
  let char_list = number |> string_of_int |> string_to_char_list in
  let len = List.length char_list in
  match mode with
  | Part1 ->
      if List.length char_list mod 2 != 0 then false
      else has_cycle_of_len (len / 2) char_list
  | Part2 -> has_cycle_of_len_up_to (len / 2) char_list

let find_bad_ids range mode =
  let start = fst range in
  let stop = snd range in
  let numbers = Seq.init (stop - start + 1) (fun x -> start + x) in
  Seq.filter (fun x -> is_bad_id x mode) numbers |> List.of_seq

let print_list xs =
  List.iter (fun x -> x |> string_of_int |> print_endline) xs;
  xs

let process_ranges input mode =
  String.split_on_char ',' input
  |> List.map (fun r ->
         match String.split_on_char '-' r with
         | [ start; stop ] -> (start |> int_of_string, stop |> int_of_string)
         | _ -> failwith "invalid range")
  |> List.fold_left
       (fun acc range -> List.append acc (find_bad_ids range mode))
       []
  |> List.fold_left (fun acc num -> acc + num) 0

let load_data mode =
  let lines = Advent.read_lines file in
  match lines with
  | [ line ] -> process_ranges line mode
  | _ -> failwith "invalid input data"

let run () =
  load_data Part1 |> string_of_int |> print_endline;
  load_data Part2 |> string_of_int |> print_endline
