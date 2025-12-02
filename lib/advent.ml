let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
  |> List.filter (fun s -> String.length (String.trim s) > 0)

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y
