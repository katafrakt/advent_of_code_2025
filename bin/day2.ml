let file = "inputs/day2.in"
let string_to_char_list s = s |> String.to_seq |> List.of_seq

let char_list_to_int =
  List.fold_left (fun acc c -> (acc * 10) + Char.code c - Char.code '0') 0

let rec has_cycle_of_len len list =
  if len = 0 then false
  else if len + 1 > List.length list then true
  else List.hd list = List.nth list len && has_cycle_of_len len (List.tl list)

let is_bad_id number =
  let char_list = number |> string_of_int |> string_to_char_list in
  let len = List.length char_list in
  if List.length char_list mod 2 != 0 then false
  else has_cycle_of_len (len / 2) char_list

let find_bad_ids range =
  let start = fst range in
  let stop = snd range in
  let numbers = Seq.init (stop - start + 1) (fun x -> start + x) in
  Seq.filter is_bad_id numbers |> List.of_seq

let print_list xs =
  List.iter (fun x -> x |> string_of_int |> print_endline) xs;
  xs

let process_ranges input =
  String.split_on_char ',' input
  |> List.map (fun r ->
         match String.split_on_char '-' r with
         | [ start; stop ] -> (start |> int_of_string, stop |> int_of_string)
         | _ -> failwith "invalid range")
  |> List.fold_left (fun acc range -> List.append acc (find_bad_ids range)) []
  |> List.fold_left (fun acc num -> acc + num) 0

let load_data () =
  let lines = Advent.read_lines file in
  match lines with
  | [ line ] -> process_ranges line
  | _ -> failwith "invalid input data"

let run () = load_data () |> string_of_int |> print_endline
