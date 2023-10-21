(* let () = print_endline "Hello, World!" *)

let () =
  match Lexer.test_next_token () with
  | Error err -> print_endline err
  | Ok is_success ->
    if is_success
    then print_endline "\nDeu certo!"
    else print_endline "\nDeu ruim!" 