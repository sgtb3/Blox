type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | FRAMEEQ
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | DOT
  | IF
  | ELSE
  | FOR
  | INT
  | BOOL
  | VOID
  | BUILD
  | JOIN
  | FRAME
  | SET
  | EOF
  | LTN
  | GTN
  | ID of (string)
  | STRING of (string)
  | FLOAT of (float)
  | LITERAL of (int)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
