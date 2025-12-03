let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun s -> String.length (String.trim s) > 0)

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

module ListExt = struct
  let rec take n xs =
    match (n, xs) with
    | 0, _ -> []
    | _, [] -> []
    | n, x :: xs -> x :: take (n - 1) xs

  let rec drop n xs =
    if List.length xs < n then xs
    else if n = 0 then xs
    else drop (n - 1) (List.tl xs)
end
