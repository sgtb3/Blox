%{ open Ast %}

%token ASSIGN COMMA SEMI
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MOD
%token NOT DOT
%token LT GT EQ NEQ LEQ GEQ FRAMEEQ AND OR
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE 
%token VOID INT BOOL STRING FLOAT
%token TRUE FALSE NULL EOF
%token PRINT BUILD JOIN FRAME SET FACE CONVERT
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

%start program                 
%type <Ast.program> program    

%%
program:
  decls EOF { $1 }

decls:
  |/* nothing */    { ([], [] )}
  | decls globals   { ($2 :: fst $1), snd $1 }
  | decls func_decl { fst $1, ($2 :: snd $1) }

dtype:
  | INT    { Int    }
  | FLOAT  { Float  }
  | BOOL   { Bool   }
  | STRING { String }
  | VOID   { Void   }
  | FRAME LT LIT_INT COMMA LIT_INT COMMA LIT_INT GT  { Frame($3,$5,$7) }
  | FACE LT LIT_INT COMMA LIT_INT COMMA LIT_INT COMMA ID GT { FaceId($3,$5,$7,$9) }
 /* | fr_decl {  $1  } */
  | dtype LBRACK LIT_INT RBRACK ID { Array($1, $3, $5) }

globals:
  | dtype ID SEMI                    /* var decls [($2, $3) :: 1]; */
    { { var_decls  = [($1, $2)]; 
        var_assgns = []; 
        fr_assgns  = []; } }
  | dtype ID ASSIGN expr SEMI        /* var assigns */
    { { var_decls  = []; 
        var_assgns = [($1, $2, $4)]; 
        fr_assgns  = []; } }
  | FRAME ID ASSIGN ID SEMI        /* fr assigns  */
    { { var_decls  = []; 
        var_assgns = []; 
        fr_assgns  = [($2, $4)]; } }

func_decl:
  dtype ID LPAREN formals_opt RPAREN LCURL stmt_list RCURL
    { { typ     = $1;
        fname   = $2;
        formals = $4;
        body    = List.rev $7 } }

formals_opt:
  |/* nothing */{ [] }
  | formal_list { List.rev $1 }

formal_list:
  | dtype ID  { [($1,$2)] }
  | formal_list COMMA dtype ID { ($3,$4) :: $1 }
/*
fr_decl:
  FRAME LT LIT_INT COMMA LIT_INT COMMA LIT_INT GT ID 
    { { fr_x    = $3; 
        fr_y    = $5; 
        fr_z    = $7; 
        fr_name = $9; } }

fc_decl:
  FACE LT LIT_INT COMMA LIT_INT COMMA LIT_INT COMMA ID GT ID 
    { { fcd_dim  = ($3, $5, $7); 
        fcd_face = $9; 
        fcd_name = $11; } }
*/
stmt_list:
  |/* nothing */   { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI              { Expr($1)           }
  | BREAK SEMI             { Break              }
  | CONTINUE SEMI          { Continue           }
  | dtype ID SEMI          { Var_decl($1,$2)    }
  | CONVERT LPAREN ID RPAREN SEMI     { Convert($3)       }
  | dtype LBRACK LIT_INT RBRACK ID SEMI { Array($1, $3, $5) }
 /* | fr_decl SEMI           { Fr_decl($1)        }
  | fc_decl SEMI           { Fc_decl($1)        } */
  | LCURL stmt_list RCURL  { Block(List.rev $2) }
  | RETURN SEMI            { Return Noexpr      }
  | RETURN expr SEMI       { Return $2          }
  | JOIN LPAREN ID COMMA ID COMMA ID COMMA ID RPAREN SEMI 
    { Join($3,$5,$7,$9) }
  | BUILD LPAREN ID COMMA ID COMMA ID COMMA ID RPAREN SEMI
    { Build($3,$5,$7,$9) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE 
    { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
    { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt 
    { While($3, $5) }

expr_opt:
  |/* nothing */{ Noexpr }
  | expr        { $1 }

expr:
  | ID                     { Id($1)                 }
  | LIT_INT                { Lit_Int($1)            }
  | LIT_FLT                { Lit_Flt($1)            }
  | LIT_STR                { Lit_Str($1)            }
  | TRUE                   { Lit_Bool(true)         }
  | FALSE                  { Lit_Bool(false)        }
  | ID ASSIGN expr         { Assign($1, $3)         }
  | FRAME ID ASSIGN expr   { Fr_assign($2, $4)      }
  | dtype ID ASSIGN expr   { Var_assign($1, $2, $4) }
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
  | /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
  | expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
