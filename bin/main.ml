let run_repl () =
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
  run_repl ()