/* ocamlyacc Parser for Blox */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token FRAMEEQ EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token DOT COLON
<<<<<<< HEAD
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
=======
%token RETURN IF ELSE FOR WHILE INT VOID BOOL
>>>>>>> refs/remotes/origin/master
%token BREAK CONTINUE
%token BUILD JOIN 
%token STRING FRAME SET MAP ARRAY
%token EOF

%token <string> ID
%token <string> STRING
%token <float> FLOAT
%token <int> LITERAL

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left DOT
%left OR
%left AND
%left EQ NEQ FRAMEEQ
%left LT GT LEQ GEQ
%left MOD
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], []                 }
 | decls vdecl   { ($2 :: fst $1), snd $1 }
 | decls fdecl   { fst $1, ($2 :: snd $1) }

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    {
      { typ     = $1;
        fname   = $2;
        formals = $4;
        locals  = List.rev $7;
        body    = List.rev $8
      }
    }

formals_opt:
    /* nothing */ { []          }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1, $2)]     }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ: 
  INT     { Int    }
| BOOL    { Bool   }
| VOID    { Void   }
| FLOAT   { Float  }
/*| BLCK    { Blck   }*/
| STRING  { String }
| FRAME   { Frame  }
/*| ARRAY   { Array  }*/
| SET     { Set of typ    }
| MAP     { Map    }

typedef_list:
    typedef {[$1]}
    | typedef_list COMMA typedef {$3::$1}

typedef_list_opt:
    /* nothing*/ {[]}
    | typedef_list {List.rev $1}

/*typedef description*/
typedef:
    ID {
        match $1 with
        | "Int" -> Int
        | "Bool" -> Bool
        | "Void" -> Void
        | "String" -> String
        | "Float" -> Float
        | "Map" | "Set" -> failwith ("set map init must with parameters")
        | x -> Class x
    }
    | ID LT typedef_list_opt GT {
        match $1 with
        | "Set" -> begin
                match $3 with
                |[x] -> Set x
                | _ -> failwith ("set just with one parameter")
                end
        | "Map" -> begin
                match $3 with
                | [x;y] -> Map (x,y)
                | _ -> failwith ("map just two parameter")
                end
        | "Array" -> begin
               match $3 with
               |[x] -> Array x
               | _ -> failwith ("array just with one parameter")
               end
        | _ -> failwith ("not suppport template except set map")
    }

/* for map */
expr_pair_list:
    /*nothing*/ {[]}
    | expr_pair_true_list {$1}
expr_pair_true_list:
    | expr COLON expr {[($1, $3)]}
    | expr_pair_true_list COMMA expr COLON expr {($3,$5)::$1}

/* for set */
expr_list_set:
    /* nothing */       { [] }
  | expr_true_list_set  { $1 }

expr_true_list_set:
    expr                    { [$1]     }
  | expr_true_list_set COMMA expr { $3 :: $1 }

/* for function call */
actuals_opt:
    /* nothing */ { []          }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1]     }
  | actuals_list COMMA expr { $3 :: $1 }

map:
    MAP LPAREN expr_pair_list RPAREN {Map(List.rev $3)}
set:
    SET LT expr_list_set RPAREN {Set(List.rev $3)}
arr:
    LBRACE expr_list_set RBRACE {Array (List.rev $2)}

vdecl_list:
    /* nothing */    { []       }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { []       }
  | stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI                                 { Expr $1               }
  | RETURN SEMI                             { Return Noexpr         }
  | RETURN expr SEMI                        { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1     }

expr:
    LITERAL                      { Literal($1)            }
  | FLOAT                        { Float($1)              }/* check this later*/
  | STRING                       { String($1)             }
  | ID                           { Id($1)                 }
  | TRUE                         { BoolLit(true)          }
  | FALSE                        { BoolLit(false)         }
  | set                          { $1                     }
  | map                          { $1                     }
  | arr                          { $1                     }
  | expr PLUS    expr            { Binop($1, Add,     $3) }
  | expr MINUS   expr            { Binop($1, Sub,     $3) }
  | expr TIMES   expr            { Binop($1, Mult,    $3) }
  | expr DIVIDE  expr            { Binop($1, Div,     $3) }
  | expr MOD     expr            { Binop($1, Mod,     $3) }
  | expr EQ      expr            { Binop($1, Equal,   $3) }
  | expr NEQ     expr            { Binop($1, Neq,     $3) }
  | expr LT      expr            { Binop($1, Less,    $3) }
  | expr LEQ     expr            { Binop($1, Leq,     $3) }
  | expr GT      expr            { Binop($1, Greater, $3) }
  | expr GEQ     expr            { Binop($1, Geq,     $3) } 
  | ID FRAMEEQ ID                { Binop($1, FrameEq, $3) } 
  | expr AND     expr            { Binop($1, And,     $3) }
  | expr OR      expr            { Binop($1, Or,      $3) }
  | MINUS expr %prec NEG         { Unop(Neg, $2)          }
  | ID DOT ID                    { Objid($1, $3)          }
  | NOT expr                     { Unop(Not, $2)          }
  | ID ASSIGN expr               { Assign($1, $3)         }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3)           }
  | LPAREN expr RPAREN           { $2                     }
