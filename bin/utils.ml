let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let string_to_char_list s =
  s |> String.to_seq |> List.of_seq