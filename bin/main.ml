(* let () =
  match Lexer.test_next_token () with
  | Ok result ->
    if result then
      print_endline "sucesso"
    else
      print_endline "falha"
  | Error _ -> print_endline "Erro"  *)

let () =
  if Parser.test_let_statements () then
    print_endline "sucesso!"
  else
    print_endline "Erro!"

(* let run_repl () =
  let rec repl () =
    let () = print_string "> " in
    let line = read_line () in
    let () = 
      match Lexer.generate_tokens line with
      | Error err -> Printf.eprintf "%s\n" err
      | Ok tokens -> 
        tokens 
        |> List.map Token.string_of_token  
        |> List.iter print_endline
    in
    repl ()
  in

  repl ()

let () =
  run_repl () *)