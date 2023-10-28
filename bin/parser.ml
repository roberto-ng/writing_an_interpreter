type t = {
  lexer: Lexer.t;
  cur_token: Token.t;
  peek_token: Token.t;
  errors: string list;
}

type expression =
  | Identifier of Token.t

type statement =
  | LetStatement of Token.t * expression
  | ReturnStatement of Token.t

type node =
  | Statement of statement
  | Expression of expression
  | Program of node list

let next_token parser =
  let (next_lexer, next_token) = 
    parser.lexer 
    |> Lexer.next_char 
  in
  {
    parser with
    lexer = next_lexer;
    cur_token = parser.peek_token;
    peek_token = next_token;
  }

let init_parser lexer =
  {
    lexer = lexer;
    cur_token = Token.Illegal;
    peek_token = Token.Illegal;
    errors = [];
  }
  |> next_token
  |> next_token

let rec token_of_node node =
  match node with
  | Statement stmt -> (      
      match stmt with
      | LetStatement (token, _expr) -> token
      | ReturnStatement (token) -> token
  )
  | Expression expr -> (
    match expr with
    | Identifier token -> token
  )
  | Program statements -> 
    if (List.length statements) > 0 then
      statements
      |> List.hd
      |> token_of_node
    else
      Token.Eof

let peek_error token_name parser =
  let error_msg = 
    Printf.sprintf 
      "Expected next token to be %s, got %s instead"  
      token_name
      (Token.Variants.to_name parser.peek_token)
  in
  {
    parser with
    errors = parser.errors @ [error_msg]
  }
            

let get_node_token_literal node =
  node
  |> token_of_node
  |> Token.get_literal

let expect_peek variant parser =
  if parser.peek_token |> Token.is_variant variant then
    (next_token parser, true)
  else
    let parser_with_error = parser |> peek_error variant.name in
    (parser_with_error, false)

let is_current_token variant parser =
  if parser.cur_token |> Token.is_variant variant then
    true
  else
    false

let parse_let_statement parser =
  let first_token = parser.cur_token in
  let (parser, is_peek_token_ident) = 
    parser |> expect_peek Token.Variants.ident
  in

  if is_peek_token_ident then
    let stmt = LetStatement (first_token, Identifier (parser.cur_token)) in
  
    let (parser, is_peek_token_assign) = 
      parser |> expect_peek Token.Variants.assign
    in
    if is_peek_token_assign then
      let rec loop parser =
        if parser |> is_current_token Token.Variants.semicolon  then
          parser
        else
          loop (parser |> next_token) 
      in
      ((loop parser), Some stmt)
    else
      (parser, None)
  else
    (parser, None)

let parse_return_statement parser =
  let stmt = ReturnStatement parser.cur_token in
  let parser = parser |> next_token in
  let rec loop parser =
    if parser |> is_current_token Token.Variants.semicolon  then
      parser
    else
      loop (parser |> next_token) 
  in
  ((loop parser), Some stmt)

let parse_statement parser =
  match parser.cur_token with
    | Token.Let -> parse_let_statement parser
    | Token.Return -> parse_return_statement parser
    | _ -> (parser, None)
      
let parse_program parser =
  let rec iter parser program_statements =
    if parser.cur_token = Token.Eof then
      (parser, List.rev program_statements)
    else
      let (parser, result) = parse_statement parser in
      let program_statements =
        match result with
        | None -> program_statements
        | Some stmt -> ((Statement stmt) :: program_statements)
      in
      iter (next_token parser) program_statements
  in
  iter parser []

let test_let_statements () =
  let input = 
    "
      return 5;
      return 10;
      return 993322;
    "
  in

  let (parser, node_list) = 
    input
    |> Lexer.new_lexer 
    |> init_parser 
    |> parse_program
  in

  let () =
    parser.errors
    |> List.iter (fun err -> Printf.eprintf "Error: %s\n" err) 
  in

  (* let () =
    node_list
    |> List.map get_node_token_literal
    |> List.iter (fun item ->
      match item with
      | Some str -> print_endline str
      | None -> print_endline "none")
  in *)
  
  (* let expected_nodes_list =
    [
      Some "x";
      Some "y";
      Some "foobar";
    ]
  in *)

  (List.length node_list) = 3