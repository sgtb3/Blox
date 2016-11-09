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
  | CREATE
  | BUILD
  | JOIN
  | FRAME
  | SET
  | MAP
  | EOF
  | ID of (string)
  | STRINGLIT of (string)
  | FLOATLIT of (float)
  | INTLIT of (int)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
