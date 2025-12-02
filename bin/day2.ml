let file = "inputs/day2.in"
let string_to_char_list s = s |> String.to_seq |> List.of_seq

let char_list_to_int =
  List.fold_left (fun acc c -> (acc * 10) + Char.code c - Char.code '0') 0

let rec take n xs =
  match (n, xs) with
  | 0, _ -> []
  | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs

let break_list_in_half lst =
  let len = List.length lst in
  let first = take (len / 2) lst in
  let second = lst |> List.rev |> take (len / 2) |> List.rev in
  (first, second)

let is_bad_id number =
  let str = string_of_int number in
  match String.length str mod 2 with
  | 0 ->
      let pair =
        number |> string_of_int |> string_to_char_list |> break_list_in_half
      in
      fst pair = snd pair
  | _ -> false

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
  |> print_list
  |> List.fold_left (fun acc num -> acc + num) 0

let load_data () =
  let lines = Advent.read_lines file in
  match lines with
  | [ line ] -> process_ranges line
  | _ -> failwith "invalid input data"

let run () = load_data () |> string_of_int |> print_endline
