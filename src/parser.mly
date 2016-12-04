%{ open Ast %}
  
%token ASSIGN COMMA SEMI COLON
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MOD
%token NOT DOT
%token LT GT LEQ GEQ FRAMEEQ AND OR
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE 
%token VOID INT BOOL STRING 
%token TRUE FALSE NULL EOF
%token PRINT BUILD JOIN FRAME SET MAP
%token <string> ID
%token <string> LIT_STR
%token <float> LIT_FLT
%token <int> LIT_INT

/* the start symbol of the grammar */
%start program                 

/* the type of the start symbol - this is 'type program' from the AST */
%type <Ast.program> program    

%%
program:
  decls EOF { $1 }

decls:
 |/* nothing */{ [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

/* add arry of typ set of typ map of typ * typ from AST here */
typ:
  | INT    { Int    }
  | BOOL   { Bool   }
  | STRING { String }
  | VOID   { Void   }

vdecl:
   typ ID SEMI { ($1, $2) }

vdecl_list:
  |/* nothing */     { [] }
  | vdecl_list vdecl { $2 :: $1 }

fdecl:
  typ ID LPAREN formals_opt RPAREN LCURL vdecl_list stmt_list RCURL
  { { typ     = $1;
      fname   = $2;
      formals = $4;
      locals  = List.rev $7;
      body    = List.rev $8 } }

formals_opt:
  |/* nothing */{ [] }
  | formal_list { List.rev $1 }

formal_list:
  | typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }


stmt_list:
  |/* nothing */   { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI     { Expr($1) }
  | PRINT ID SEMI { Fr_print($2) }
  | JOIN LPAREN join_arg COMMA join_arg RPAREN SEMI { Join($3,$5) }
  | FRAME LT LIT_INT COMMA LIT_INT COMMA LIT_INT GT ID SEMI { Fr_decl($3,$5,$7,$9) }
  | LCURL stmt_list RCURL { Block(List.rev $2) }
  | BREAK SEMI    { Break    }
  | CONTINUE SEMI { Continue }

expr:
  | ID                     { Id($1)          }
  | LIT_INT                { Lit_Int($1)     }
  | TRUE                   { Lit_Bool(true)  }
  | FALSE                  { Lit_Bool(false) }
  | FRAME ID ASSIGN expr   { Assign($2, $4)  }
  | ID ASSIGN expr         { Assign($1, $3)  }

join_arg:
  ID COMMA LCURL face_set RCURL
  { { frname = $1; blck_face = $4; } }

face_set:
  | face_id                  { [$1] }
  | face_set COMMA face_id   { $3 :: $1 }

face_id:
  LPAREN LIT_INT COMMA LIT_INT COMMA LIT_INT COMMA ID RPAREN
  { { dim = ($2,$4,$6); face = $8; } }
  