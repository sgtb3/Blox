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
    /* nothing */         { [] }
  | stmt_list stmt  { $2 :: $1 }


/* should be the same as type prim in AST */
stmt:
    expr SEMI         { Expr($1)     }
  | JOIN LPAREN join_arg COMMA join_arg RPAREN SEMI { Join($3,$5) }
  | FRAME FOPEN INTLIT COMMA INTLIT COMMA INTLIT FCLOSE ID SEMI { Fr_decl($3,$5,$7,$9) }
  | PRINT ID SEMI { Fr_print($2) }

	/*| join              { Join($1)       }
  | fr_decl           { Fr_decl($1)  }
  | fr_print          { Fr_print($1) }*/

expr:
  | INTLIT           { Int($1)        }
  | ID               { Id($1)         }
  | ID ASSIGN expr   { Assign($1, $3) }
/*
fr_print:
  PRINT ID SEMI
  { { fr_id = $2; } }

fr_decl:
  FRAME FOPEN INTLIT COMMA INTLIT COMMA INTLIT FCLOSE ID SEMI 
  { { x = $3; 
      y = $5;
      z = $7;
      fr_name = $9; } }
	
join:
  JOIN LPAREN join_arg COMMA join_arg RPAREN SEMI
  { { fr_a = $3;
      fr_b = $5; } }
*/
join_arg:
	ID COMMA LCURL face_set RCURL
  { { fr_name = $1;
      blck_face = $4; } }

face_set:
	  face_id                  { [$1]     }
	|	face_set COMMA face_id   { $3 :: $1 }

face_id:
	LPAREN INTLIT COMMA INTLIT COMMA INTLIT COMMA ID RPAREN
  { { dim = ($2,$4,$6); face = $8; } }
	