%{ open Ast %}
  
%token FOPEN FCLOSE COMMA SEMI
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
  fr_declar_list EOF { $1 }

fr_declar_list:
  | /* nothing */           { [] }
  | fr_declar_list fr_declar { $2 :: $1 }

fr_declar:
  FRAME FOPEN INTLIT COMMA INTLIT COMMA INTLIT FCLOSE ID SEMI 
  { { x = $3; 
      y = $5;
      z = $7;
      fr_name = $9; } }

/* should be the same as type prim in AST */
stmt:
  | expr SEMI { Expr($1) }

expr:
  | INTLIT   { Int($1) }
  | ID { Id($1)  }
