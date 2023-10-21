open Token

let ( let* ) = Option.bind
let ( let+ ) = Result.bind


type t = {
  input: string;
  (* current position in input (points to current char) *)
  position: int;
  (* current reading position in input (after current char) *)
  read_position: int;
  (* current char under examination *)
  ch: char; 
}

let read_char lexer =
  let input_len = lexer.input |> String.length in
  let new_ch = 
    if lexer.read_position >= input_len 
    then '\x00'
    else lexer.input.[lexer.read_position]
  in
  {
    lexer with 
    ch = new_ch; 
    position = lexer.read_position;
    read_position = lexer.read_position + 1
  }

let new_lexer input =
  let lexer = 
    {
      input = input;
      position = 0;
      read_position = 0;
      ch = '\x00';
    }
  in
  read_char lexer

let next_token lexer =
  let character = lexer.ch in
  
  match character with
  | '=' -> Some Token.Assign
  | ';' -> Some Token.Semicolon
  | '(' -> Some Token.LParen
  | ')' -> Some Token.RParen
  | ',' -> Some Comma
  | '+' -> Some Plus
  | '{' -> Some LBrace
  | '}' -> Some RBrace
  |'\x00' -> Some Eof
  | _ -> None
  
let advance lexer =
  let new_token = next_token lexer in
  let new_lexer = read_char lexer in
  (new_lexer, new_token) 

let next_char lexer =
  match next_token lexer with
  | None -> Error "Unmatched character"
  | Some token ->
    if token == Eof 
    then Ok (lexer, token)
    else Ok (read_char lexer, token)

let generate_tokens input_string =
  let lexer = new_lexer input_string in
  let rec iter lexer tokens =
    let+ result = next_char lexer in 
    match result with
    | (_, Token.Eof) -> Ok (List.rev_append tokens [Token.Eof])
    | (new_lexer, token) -> iter new_lexer (token :: tokens)
  in
  iter lexer []

let test_next_token () =
  let input = "=+(){},;" in

  let tests = 
    [ 
      Token.Assign; 
      Token.Plus; 
      Token.LParen; 
      Token.RParen;
      Token.LBrace;
      Token.RBrace;
      Token.Comma;
      Token.Semicolon; 
      Token.Eof;
    ]
  in

  let+ result = generate_tokens input in
  let () =
    result
    |> List.iter Token.print
  in
  Ok (tests = result)