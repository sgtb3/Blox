type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | MOD
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
  | COLON
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | BREAK
  | CONTINUE
  | BUILD
  | JOIN
  | FRAME
  | SET
  | MAP
  | AT
  | EOF
  | ID of (string)
  | STRING of (string)
  | FLOAT of (float)
  | LITERAL of (int)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
