%{ open Ast %}
  
%token FOPEN FCLOSE COMMA SEMI ASSIGN
%token FRAME VOID PRINT JOIN
%token LCURL RCURL LPAREN RPAREN
%token EOF
%token <string> ID
%token <int> INTLIT

/* the start symbol of the grammar */
%start program                 

/* the type of the start symbol - this is 'type program' from the AST */
%type <Ast.program> program    

%%
program:
  stmt_list EOF { $1 }

stmt_list:
    /* nothing */   { [] }
  | stmt_list stmt  { $2 :: $1 }

stmt:
    expr SEMI     { Expr($1) }
  | PRINT ID SEMI { Fr_print($2) }
  | JOIN LPAREN join_arg COMMA join_arg RPAREN SEMI { Join($3,$5) }
  | FRAME FOPEN INTLIT COMMA INTLIT COMMA INTLIT FCLOSE ID SEMI { Fr_decl($3,$5,$7,$9) }
  | LCURL stmt_list RCURL { Block(List.rev $2) }

expr:
  | ID                   { Id($1)  }
  | INTLIT               { Int($1) }
  | FRAME ID ASSIGN expr { Assign($2, $4) }

join_arg:
	ID COMMA LCURL face_set RCURL
  { { frname = $1; blck_face = $4; } }

face_set:
	  face_id                  { [$1] }
	|	face_set COMMA face_id   { $3 :: $1 }

face_id:
	LPAREN INTLIT COMMA INTLIT COMMA INTLIT COMMA ID RPAREN
  { { dim = ($2,$4,$6); face = $8; } }
	