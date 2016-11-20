type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LT
  | GT
  | COMMA
  | ASSIGN
  | PRINT
  | INT
  | JOIN
  | FRAME
  | EOF
  | ID of (string)
  | LITERAL of (int)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
