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
  | Statement of Token.t
  | Expression of Token.t
  | Program of node list

let next_token parser =
  let (next_lexer, next_token) = parser.lexer |> Lexer.next_token in
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

let rec get_node_token_literal node =
  match node with
  | Statement token -> Token.string_of_token token
  | Expression token -> Token.string_of_token token
  | Program tokens ->
    if (List.length tokens) > 0 then
      tokens
      |> List.hd
      |> get_node_token_literal
    else
      ""