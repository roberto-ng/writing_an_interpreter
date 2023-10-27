type t = {
  lexer: Lexer.t;
  cur_token: Token.t;
  peek_token: Token.t;
}

type expression =
  | Identifier of Token.t

type statement =
  | LetStatement of Token.t * expression

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
    lexer = next_lexer;
    cur_token = parser.peek_token;
    peek_token = next_token;
  }

let init_parser lexer =
  {
    lexer = lexer;
    cur_token = Token.Illegal;
    peek_token = Token.Illegal;
  }
  |> next_token
  |> next_token

let rec token_of_node node =
  match node with
  | Statement stmt ->
    let token = 
      match stmt with
      | LetStatement (_token, expr) -> 
        let identifier =
          match expr with
          | Identifier ident ->  ident
        in
        identifier
    in
    token
  | Expression expr ->
    let token =
      match expr with
      | Identifier token -> token
    in
    token
  | Program statements -> 
    if (List.length statements) > 0 then
      statements
      |> List.hd
      |> token_of_node
    else
      Token.Eof


let get_node_token_literal node =
  node
  |> token_of_node
  |> Token.get_literal

let expect_peek token parser =
  if parser.peek_token = token then
    (next_token parser, true)
  else
    (parser, false)
      
let parse_let_statement parser =
  let first_token = parser.cur_token in
  match parser.peek_token with
  | Token.Ident ident  ->
    let parser = next_token parser in
    let stmt = LetStatement (first_token, Identifier (Token.Ident ident)) in
    if (parser.peek_token = Token.Assign) then
      let parser = next_token parser in
      let rec loop parser =
        let parser = next_token parser in
        if parser.cur_token = Token.Semicolon then
          parser
        else
          loop parser 
      in
      ((loop parser), Some stmt)
    else
      (parser, None)
  | _ -> (parser, None)

let parse_statement parser =
  match parser.cur_token with
    | Token.Let -> parse_let_statement parser
    | _ -> (parser, None)
      
let parse_program parser =
  let rec iter parser program_statements =
    match parser.cur_token with
    | Token.Eof -> (List.rev program_statements)
    | _ ->
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
      let x = 5;
      let y = 10;
      let foobar = 838383;
    "
  in

  let node_list = 
    input
    |> Lexer.new_lexer 
    |> init_parser 
    |> parse_program
    |> List.map get_node_token_literal
  in

  let () =
    node_list
    |> List.iter (fun item ->
      match item with
      | Some str -> print_endline str
      | None -> print_endline "none")
  in
  
  let expected_nodes_list =
    [
      Some "x";
      Some "y";
      Some "foobar";
    ]
  in

  node_list = expected_nodes_list