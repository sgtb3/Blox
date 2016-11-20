/* ocamlyacc Parser for Blox */

%{ open Ast %}

/*
	%token LBRACE RBRACE LBRACK RBRACK 
	%token PLUS MINUS TIMES DIVIDE NOT
	%token FRAMEEQ EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
	%token LTN GTN
	%token DOT
	%token IF ELSE FOR BOOL VOID
	%token BUILD SET
	%token <string> STRING
	%nonassoc NOELSE
	%nonassoc ELSE
	%left OR
	%left AND
	%left EQ NEQ FRAMEEQ
	%left LT GT LEQ GEQ
	%left PLUS MINUS
	%left TIMES DIVIDE
	%right NOT NEG
*/	
	
%token SEMI LPAREN RPAREN COMMA
%token ASSIGN 
%token INT
%token JOIN 
%token FRAME
%token EOF
%token <string> ID
%token <int> LITERAL
%right ASSIGN
%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], []                 }
 | decls vdecl   { ($2 :: fst $1), snd $1 }

formals_opt:
    /* nothing */ { []          }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1, $2)]     }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
   INT  { Int   }
  |BOOL { Bool  }
  |FRAME{ Frame }

typedef_list:
    typedef {[$1]}
    | typedef_list COMMA typedef {$3::$1}

typedef_list_opt:
    /*nothing*/ {[]}
    | typedef_list {List.rev $1}

/*typedef description*/
typedef:
    ID {
        match $1 with
        | "Int" -> Int
        | "Bool" -> Bool
        | "Void" -> Void
        | "String" -> String
        | "Set" -> failwith ("set map init must with parameters") 
				| x -> Default x 
    }
    | ID LTN typedef_list_opt GTN {
        match $1 with
        | "Set" -> begin
                match $3 with
                |[x] -> Set x
                | _ -> failwith ("set just with one parameter")
                end
        | "Array" -> begin
               match $3 with
               |[x] -> Array x
               | _ -> failwith ("array just with one parameter")
               end
        | _ -> failwith ("not suppport template except set map")
    }

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

set:
    SET LPAREN expr_list_set RPAREN {Set(List.rev $3)}
arr:
    LBRACK expr_list_set RBRACK {Array (List.rev $2)}

vdecl_list:
    /* nothing */    { []       }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /*nothing*/ {[]} 
    | stmt_true_list {$1}

stmt_true_list:
    stmt SEMI {[$1]}
    | stmt SEMI stmt_true_list {$1 :: $3}

stmt:
  expr SEMI                                 { Expr $1               }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1     }

expr:
    LITERAL                      { Literal($1)            }
  | ID                           { Id($1)                 }
  | TRUE                         { BoolLit(true)          }
  | FALSE                        { BoolLit(false)         }
  | set                          { $1                     }
  | arr                          { $1                     }
  | expr PLUS    expr            { Binop($1, Add,     $3) }
  | expr MINUS   expr            { Binop($1, Sub,     $3) }
  | expr TIMES   expr            { Binop($1, Mult,    $3) }
  | expr DIVIDE  expr            { Binop($1, Div,     $3) }
  | expr EQ      expr            { Binop($1, Equal,   $3) }
  | expr NEQ     expr            { Binop($1, Neq,     $3) }
  | expr LT      expr            { Binop($1, Less,    $3) }
  | expr LEQ     expr            { Binop($1, Leq,     $3) }
  | expr GT      expr            { Binop($1, Greater, $3) }
  | expr GEQ     expr            { Binop($1, Geq,     $3) } 
  | expr FRAMEEQ expr            { Binop($1, FrameEq, $3) } 
  | expr AND     expr            { Binop($1, And,     $3) }
  | expr OR      expr            { Binop($1, Or,      $3) }
  | MINUS expr %prec NEG         { Unop(Neg, $2)          }
  | NOT expr                     { Unop(Not, $2)          }
  | ID ASSIGN expr               { Assign($1, $3)         }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3)           }
  | LPAREN expr RPAREN           { $2                     }
