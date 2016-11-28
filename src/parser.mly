%{ open Ast %}
  
%token FOPEN FCLOSE LCURL RCURL COMMA SEMI
%token FRAME VOID PRINT            
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
    stmt
  | stmt_list stmt

/* should be the same as type prim in AST */
stmt:
    expr SEMI         { Expr($1) }
	| join
  | fr_declar
  | fr_print

expr:
    VOID       { Void($1)}
  | INTLIT     { Int($1) }
  | ID         { Id($1)  }
  | fr_assign

fr_print:
  print ID SEMI

fr_declar:
  FRAME FOPEN INTLIT COMMA INTLIT COMMA INTLIT FCLOSE ID SEMI 
  { { x = $3; 
      y = $5;
      z = $7;
      fr_name = $9; } }

fr_assign:
  FRAME ID ASSIGN ID
	
join:
  JOIN LPAREN join_arg COMMA join_arg RPAREN SEMI

join_arg:
	ID COMMA LCURL face_set RCURL

face_set:
	  face_id
	|	face_set COMMA face_id

face_id:
	LPAREN INTLIT COMMA INTLIT COMMA INTLIT COMMA ID RPAREN
	