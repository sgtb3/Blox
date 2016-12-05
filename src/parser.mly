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
%type <Ast.program> program    

%%
program:
  decls EOF { $1 }

decls:
 |/* nothing */    { [], [] }
 | decls globals   { ($2 :: fst $1), snd $1 }
 | decls func_decl { fst $1, ($2 :: snd $1) }

/* add (Array of typ), (Set of typ), (Map of typ * typ) from AST here */
typ:
  | INT    { Int      }
  | BOOL   { Bool     }
  | STRING { String   }
  | VOID   { Void     }

globals:
  |                                /* no globals */ 
    { { var_decls  = []; 
        var_assgns = []; 
        fr_decls   = []; 
        fr_assgns  = []; } }
  | typ ID SEMI                    /* var decls [($2, $3) :: 1]; */
    { { var_decls  = [($1, $2)]; 
        var_assgns = []; 
        fr_decls   = []; 
        fr_assgns  = []; } }
  | typ ID ASSIGN expr SEMI        /* var assigns */
    { { var_decls  = []; 
        var_assgns = [($2, $4)]; 
        fr_decls   = []; 
        fr_assgns  = []; } }
  | fr_decl SEMI                   /* fr decls  ($2 :: $1) */    
    { { var_decls  = []; 
        var_assgns = []; 
        fr_decls   = [$1]; 
        fr_assgns  = []; } }
  | FRAME ID ASSIGN ID SEMI        /* fr assigns  */
    { { var_decls  = []; 
        var_assgns = []; 
        fr_decls   = []; 
        fr_assgns  = [($2, $4)]; } }

vdecl:
   typ ID SEMI { ($1, $2) }

vdecl_list:
  |/* nothing */       { [] }
  | vdecl_list vdecl { $2 :: $1 }

func_decl:
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
  | typ ID  { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

fr_decl:
  FRAME LT LIT_INT COMMA LIT_INT COMMA LIT_INT GT ID 
    { { x = $3; 
        y = $5; 
        z = $7; 
        fr_name = $9 } }

stmt_list:
  |/* nothing */   { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI     { Expr($1) }
  | PRINT ID SEMI { Fr_print($2) }
  | JOIN LPAREN join_arg COMMA join_arg RPAREN SEMI { Join($3,$5) }
  | LCURL stmt_list RCURL { Block(List.rev $2) }
  | BREAK SEMI    { Break    }
  | CONTINUE SEMI { Continue }
  | fr_decl SEMI  { Fr_decl($1) }

expr:
  | ID                     { Id($1)          }
  | LIT_INT                { Lit_Int($1)     }
  | TRUE                   { Lit_Bool(true)  }
  | FALSE                  { Lit_Bool(false) }
  | FRAME ID ASSIGN expr   { Assign($2, $4)  }
  | ID ASSIGN expr         { Assign($1, $3)  }

join_arg:
  ID COMMA LCURL face_set RCURL
    { { frname    = $1; 
        blck_face = $4; } }

face_set:
  | face_id                { [$1] }
  | face_set COMMA face_id { $3 :: $1 }

face_id:
  LPAREN LIT_INT COMMA LIT_INT COMMA LIT_INT COMMA ID RPAREN
    { { dim  = ($2, $4, $6); 
        face = $8; } }
  