/* ocamlyacc Parser for Blox */

%{ open Ast %}

/*
	%token LBRACE RBRACE LBRACK RBRACK 
	%token PLUS MINUS TIMES DIVIDE NOT
	%token FRAMEEQ EQ NEQ LEQ GEQ TRUE FALSE AND OR
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
	
%token SEMI LPAREN RPAREN LT GT COMMA
%token ASSIGN PRINT
%token INT
%token JOIN 										
%token FRAME
%token EOF
%token TUPLE
%token <string> ID
%token <int> LITERAL
%right ASSIGN										/* precedence level never useful */
%start program
%type <Ast.program> program

%%

program: 
  decls EOF { $1 }

decls:
   /* nothing */ { []               }
 | decls vdecl   { ($2 :: fst $1), snd $1 }

/*formals_opt:*/
	/* nothing *//* { []          }*/
/*| formal_list   { List.rev $1 }*/

formal_list:
    typ ID                   { [($1, $2)]     }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
   INT  { Int   }
  |FRAME{ Frame }	
	
vdecl_list:
    /* nothing */    { []       }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) } 

stmt_list:
    /* nothing */ {[]} 
  | stmt_list stmt { $2 :: $1 }
	 
stmt:
  expr SEMI                                { Expr $1               }
 | /* LBRACE stmt_list RBRACE                 { Block(List.rev $2)    } */
 | JOIN LPAREN expr expr expr expr RPAREN SEMI   { Join($3, $4, $5, $6) }
 | FRAME LT expr expr expr GT expr			 { Frame <$3, $4, $5> $7 }
	 
expr:
    LITERAL                      { Literal($1)            }
  | ID                           { Id($1)                 }
  | ID ASSIGN expr               { Assign($1, $3)         }
  | LPAREN expr RPAREN           { $2                     }	
	| TUPLE LPAREN expr expr expr expr RPAREN {Tuple($3, $4, $5, $6)}			
	
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }	
	
/*
typedef_list:
    typedef {[$1]}
    | typedef_list COMMA typedef {$3::$1}

typedef_list_opt:
    /nothing/ {[]}
    | typedef_list {List.rev $1}
*/
/*typedef description*/
/*
typedef:
    ID {
        match $1 with
        | "Int" -> Int
				| "Frame" -> Frame
				| x -> Default x 
    }
*/

/* for function call */
/*
actuals_opt:
    / nothing / { []          }
  | actuals_list  { List.rev $1 }
  
actuals_list:
    expr                    { [$1]     }
  | actuals_list COMMA expr { $3 :: $1 }
*/


/*		
stmt_true_list:
    stmt SEMI {[$1]}
    | stmt SEMI stmt_true_list {$1 :: $3}
*/	