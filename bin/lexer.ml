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

let rec skip_whitespace lexer =
  match lexer.ch with
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (read_char lexer)
  | _ -> lexer

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

let read_identifier lexer =
  let initial_position = lexer.position in
  let rec loop lexer =
    let last_position = lexer.position + 1 in
    let next_lexer = read_char lexer in

    (* Continue until next char isn't a letter *)
    if BatChar.is_letter next_lexer.ch then 
      loop next_lexer
    else 
      let literal =
        lexer.input 
        |> BatString.slice ~first: initial_position ~last: last_position
      in
      (lexer, literal)
  in
  loop lexer

let read_number lexer =
  let initial_position = lexer.position in
  let rec loop lexer =
    let last_position = lexer.position + 1 in
    let next_lexer = read_char lexer in

    (* Continue until next char isn't a digit *)
    if BatChar.is_digit next_lexer.ch then 
      loop next_lexer
    else 
      let literal =
        lexer.input 
        |> BatString.slice ~first: initial_position ~last: last_position
      in
      (lexer, literal)
  in
  loop lexer

let lookup_ident ident =
  match String.trim ident with
  | "let" -> Token.Let
  | "fn" -> Token.Function
  | _ -> Token.Ident ident

let next_token lexer =
  let lexer = skip_whitespace lexer in
  if BatChar.is_letter lexer.ch then
    let (lexer, literal) = read_identifier lexer in
    (lexer, lookup_ident literal)
  else if BatChar.is_digit lexer.ch then
    let (lexer, literal) = read_number lexer in
    (lexer, Token.Int literal)
  else 
    match lexer.ch with
    | '=' -> (lexer, Token.Assign)
    | ';' -> (lexer, Token.Semicolon)
    | '(' -> (lexer, Token.LParen)
    | ')' -> (lexer, Token.RParen)
    | ',' -> (lexer, Comma)
    | '+' -> (lexer, Plus)
    | '-' -> (lexer, Minus)
    | '!' -> (lexer, Bang)
    | '*' -> (lexer, Asterisk)
    | '/' -> (lexer, Slash)
    | '<' -> (lexer, LT)
    | '>' -> (lexer, GT)
    | '{' -> (lexer, LBrace)
    | '}' -> (lexer, RBrace)
    |'\x00' -> (lexer, Eof)
    | _ -> (lexer, Illegal)
  
let advance lexer =
  let new_token = next_token lexer in
  let new_lexer = read_char lexer in
  (new_lexer, new_token) 

let next_char lexer =
  let (lexer, token) = next_token lexer in
  if token == Eof then 
    Ok (lexer, token)
  else 
    Ok (read_char lexer, token)

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
  let input =
    "
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;
    "
  in

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