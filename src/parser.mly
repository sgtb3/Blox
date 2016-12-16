%{ open Ast %}
  
%token ASSIGN COMMA SEMI COLON
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MOD
%token NOT DOT
%token LT GT EQ NEQ LEQ GEQ FRAMEEQ AND OR
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE 
%token VOID INT BOOL STRING 
%token TRUE FALSE NULL EOF
%token PRINT BUILD JOIN FRAME SET
%token <string> ID
%token <string> LIT_STR
%token <float> LIT_FLT
%token <int> LIT_INT

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

/* the start symbol of the grammar */
%start program                 
%type <Ast.program> program    

%%
program:
  decls EOF { $1 }

decls:
  /* nothing */    { [], [] }
 | decls globals   { ($2 :: fst $1), snd $1 }
 | decls func_decl { fst $1, ($2 :: snd $1) }

/* add (Array of typ), (Set of typ) from AST here */
typ:
    INT    { Int    }
  | BOOL   { Bool   }
  | STRING { String }
  | VOID   { Void   }
  

/*
set:
    SET LT expr_list_set RPAREN      { Set(List.rev $3)   }
arr:
    typ LBRACK expr_list_set RBRACK  { Array(List.rev $2) }  
expr_pair_list:
  | { [] }
  | expr_pair_true_list { $1 }
expr_pair_true_list:
  | expr COLON expr { [($1,$3)] }
  | expr_pair_true_list COMMA expr COLON expr {($3,$5)::$1}
expr_list_set:
  | { [] }
  | expr_true_list_set { $1 }
expr_true_list_set:
  | expr { [$1] }
  | expr_true_list_set COMMA expr { $3 :: $1 }*/

globals:
    typ ID SEMI                    /* var decls [($2, $3) :: 1]; */
    { { var_decls  = [($1, $2)]; 
        var_assgns = []; 
        fr_decls   = []; 
        fr_assgns  = []; } }
  | typ ID ASSIGN expr SEMI        /* var assigns */
    { { var_decls  = []; 
        var_assgns = [($1, $2, $4)]; 
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
   /* nothing */     { [] }
  | vdecl_list vdecl { $2 :: $1 }

vassn:
  typ ID ASSIGN expr SEMI {($1, $2, $4)}

vassn_list:
   /* nothing */     { [] }
  | vassn_list vassn { $2 :: $1 }

func_decl:
  typ ID LPAREN formals_opt RPAREN LCURL vdecl_list vassn_list stmt_list RCURL
    { { typ     = $1;
        fname   = $2;
        formals = $4;
        loc_var_decl = List.rev $7;
        loc_var_assn = List.rev $8;
        body    = List.rev $9 } }

formals_opt:
   /* nothing */{ [] }
  | formal_list { List.rev $1 }

formal_list:
    typ ID  { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

fr_decl:
  FRAME LT LIT_INT COMMA LIT_INT COMMA LIT_INT GT ID 
    { { x = $3; 
        y = $5; 
        z = $7; 
        fr_name = $9 } }

stmt_list:
   /* nothing */   { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI              { Expr($1)           }
  | PRINT ID SEMI          { Fr_print($2)       }
  | BREAK SEMI             { Break              }
  | CONTINUE SEMI          { Continue           }
  | fr_decl SEMI           { Fr_decl($1)        }
  | LCURL stmt_list RCURL  { Block(List.rev $2) }
  | JOIN LPAREN join_arg COMMA join_arg RPAREN SEMI { Join($3,$5) }

expr:
    ID                     { Id($1)                 }
  | LIT_INT                { Lit_Int($1)            }
  | TRUE                   { Lit_Bool(true)         }
  | FALSE                  { Lit_Bool(false)        }
  | FRAME ID ASSIGN expr   { Assign($2, $4)         }
  | ID ASSIGN expr         { Assign($1, $3)         }
  | expr PLUS   expr       { Binop($1, Add,   $3)   }
  | expr MINUS  expr       { Binop($1, Sub,   $3)   }
  | expr TIMES  expr       { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr       { Binop($1, Div,   $3)   }
  | expr EQ     expr       { Binop($1, Equal, $3)   }
  | expr NEQ    expr       { Binop($1, Neq,   $3)   }
  | expr LT     expr       { Binop($1, Less,  $3)   }
  | expr LEQ    expr       { Binop($1, Leq,   $3)   }
  | expr GT     expr       { Binop($1, Greater, $3) }
  | expr GEQ    expr       { Binop($1, Geq,   $3)   }
  | expr AND    expr       { Binop($1, And,   $3)   }
  | expr OR     expr       { Binop($1, Or,    $3)   }
  | MINUS expr %prec NEG   { Unop(Neg, $2)          }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3)     }
  | LPAREN expr RPAREN           { $2               }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

join_arg:
  ID COMMA LCURL face_set RCURL
    { { frname    = $1; 
        blck_face = $4; } }

face_set:
    face_id                { [$1] }
  | face_set COMMA face_id { $3 :: $1 }

face_id:
  LPAREN LIT_INT COMMA LIT_INT COMMA LIT_INT COMMA ID RPAREN
    { { dim  = ($2, $4, $6); 
        face = $8; } }
  