%{ open Ast %}

%token ASSIGN COMMA SEMI
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MOD
%token NOT DOT
%token LT GT EQ NEQ LEQ GEQ FRAMEEQ AND OR
%token IF ELSE FOR WHILE RETURN BREAK CONTINUE
%token VOID INT BOOL STRING FLOAT
%token TRUE FALSE NULL EOF
%token PRINT BUILD JOIN FRAME FACE CONVERT
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
  |/* nothing */    { ([], []) }
  | decls globals   { ($2 :: fst $1), snd $1 }
  | decls func_decl { fst $1, ($2 :: snd $1) }

globals:
  | dtype ID SEMI             { VarDecl($1, $2)       }
  | dtype ID ASSIGN expr SEMI { VarAssign($1, $2, $4) }
  | FRAME ID ASSIGN ID SEMI   { FrAssign($2, $4)      }
  | FACE  ID ASSIGN ID SEMI   { FcAssign($2, $4)      }

func_decl:
  | dtype ID LPAREN formals_opt RPAREN LCURL stmt_list RCURL
    { { typ = $1; fname = $2; formals = $4; body = List.rev $7 } }

face_decl:
  | LT LIT_INT COMMA LIT_INT COMMA LIT_INT COMMA ID GT
    { { dim = ($2, $4, $6); face = $8; fc_id = "" } }

frame_decl:
  | LT LIT_INT COMMA LIT_INT COMMA LIT_INT GT
    { { x = $2; y = $4; z = $6; fr_id = ""; blocks = [||] } }

formals_opt:
  |/* nothing */               { []             }
  | formal_list                { List.rev $1    }
formal_list:
  | dtype ID                   { [($1, $2)]     }
  | formal_list COMMA dtype ID { ($3, $4) :: $1 }

actuals_opt:
  | /* nothing */           { []          }
  | actuals_list            { List.rev $1 }
actuals_list:
  | expr                    { [$1]        }
  | actuals_list COMMA expr { $3 :: $1    }

build_args:
  | build_args_list { List.rev $1 }
build_args_list:
  | ID
    { [{ dim = (0, 0, 0); face = "*"; fc_id = $1 }] }
  | LBRACK build_args_list COMMA ID RBRACK
    { { dim = (0, 0, 0); face = "*"; fc_id = $4 } :: $2 }

expr_opt:
  |/* nothing */ { Noexpr }
  | expr         { $1     }

stmt_list:
  |/* nothing */   { [] }
  | stmt_list stmt { $2 :: $1 }

dtype:
  | INT                            { Int                }
  | FLOAT                          { Float              }
  | BOOL                           { Bool               }
  | STRING                         { String             }
  | VOID                           { Void               }
  | FRAME frame_decl               { Frame($2)          }
  | FACE face_decl                 { FaceId($2)         }
  | dtype LBRACK LIT_INT RBRACK ID { Array($1, $3, $5)  }

stmt:
  | expr SEMI                               { Expr($1)                 }
  | BREAK SEMI                              { Break                    }
  | CONTINUE SEMI                           { Continue                 }
  | LCURL stmt_list RCURL                   { Block(List.rev $2)       }
  | RETURN SEMI                             { Return Noexpr            }
  | RETURN expr SEMI                        { Return $2                }
  | PRINT LPAREN expr RPAREN SEMI           { Print($3)                }
  | dtype LBRACK LIT_INT RBRACK ID SEMI     { Array($1, $3, $5)        }
  | dtype ID SEMI                           { Var_Decl($1, $2)         }
  | dtype ID ASSIGN expr SEMI               { Var_Assign(($1, $2), $4) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)            }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)           }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([]))    }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
    { For($3, $5, $7, $9) }
  | CONVERT LPAREN ID RPAREN SEMI
    { Convert({ x = 0; y = 0; z = 0; fr_id  = $3; blocks = [||] }) }
  | BUILD LPAREN ID COMMA build_args COMMA ID COMMA build_args RPAREN SEMI
    { Build({ x = 0; y = 0; z = 0; fr_id = $3; blocks = [||] }, $5,
            { x = 0; y = 0; z = 0; fr_id = $7; blocks = [||] }, $9) }
  | JOIN LPAREN ID COMMA ID COMMA ID COMMA ID RPAREN SEMI
    { Join( { x = 0; y = 0; z = 0; fr_id = $3; blocks = [||] },
            { dim = (0, 0, 0); face = ""; fc_id = $5 },
            { x = 0; y = 0; z = 0; fr_id  = $7; blocks = [||] },
            { dim = (0, 0, 0); face = ""; fc_id = $9 }) }
expr:
  | ID                     { Id($1)                 }
  | LIT_INT                { Lit_Int($1)            }
  | LIT_FLT                { Lit_Flt($1)            }
  | LIT_STR                { Lit_Str($1)            }
  | TRUE                   { Lit_Bool(true)         }
  | FALSE                  { Lit_Bool(false)        }
  | ID ASSIGN expr         { Assign($1, $3)         }
  | FRAME ID ASSIGN expr   { Fr_Assign($2, $4)      }
  | FACE ID ASSIGN expr    { Fc_Assign($2, $4)      }
  | expr PLUS   expr       { Binop($1, Add, $3)     }
  | expr MINUS  expr       { Binop($1, Sub, $3)     }
  | expr TIMES  expr       { Binop($1, Mult, $3)    }
  | expr DIVIDE expr       { Binop($1, Div, $3)     }
  | expr EQ     expr       { Binop($1, Equal, $3)   }
  | expr NEQ    expr       { Binop($1, Neq, $3)     }
  | expr LT     expr       { Binop($1, Less, $3)    }
  | expr LEQ    expr       { Binop($1, Leq, $3)     }
  | expr GT     expr       { Binop($1, Greater, $3) }
  | expr GEQ    expr       { Binop($1, Geq, $3)     }
  /*| expr AND    expr       { Binop($1, And, $3)     }*/
  /*| expr OR     expr       { Binop($1, Or, $3)      }*/
  | MINUS expr %prec NEG   { Unop(Neg, $2)          }
  | LPAREN expr RPAREN     { $2                     }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3)     }
