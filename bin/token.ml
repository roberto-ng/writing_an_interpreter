let ( let* ) = Option.bind

type t =
  | Illegal
  | Eof
   (* Identifiers + literals *)
  | Ident of string
  | Int of string
  (* Operators *)
  | Assign
  | Plus
  (* Delimiters *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* Keywords *)
  | Function
  | Let

let token_of_string str value = 
  match str with
  | "ILLEGAL" -> Some Illegal
  | "EOF" -> Some Eof
  | "IDENT" -> 
    let* ident_value = value in
    Some (Ident ident_value)
  | "INT" -> 
    let* int_value = value in
    Some (Ident int_value)
  | "=" -> Some Assign
  | "+" -> Some Plus
  | "," -> Some Comma
  | ";" -> Some Semicolon
  | "(" -> Some LParen
  | ")" -> Some RParen
  | "{" -> Some LBrace
  | "}" -> Some RBrace
  | "FUNCTION" -> Some Function
  | "LET" -> Some Let
  | _ -> None

let string_of_token token =
  match token with
  | Illegal -> "ILLEGAL"
  | Eof -> "EOF"
  | Ident identifier -> "IDENT " ^ identifier
  | Int value -> "INT " ^ value
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | LParen -> "LPAREN"
  | RParen -> "RPAREN"
  | LBrace -> "LBRACE"
  | RBrace -> "RBRACE"
  | Function -> "FUNCTION"
  | Let -> "LET"

let print token =
  token 
  |> string_of_token 
  |> print_endline