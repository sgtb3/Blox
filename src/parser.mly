/* ocamlyacc Parser for Blox */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA 
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT 
%token BLOCKEQ EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR 
%token RETURN IF ELSE FOR WHILE INT BOOL VOID 
%token TRUE FALSE BREAK NULL
%token CREATE BUILD RULE JOIN DETACH FRAME

%token <int> LITERAL 
%token <string> ID 
%token EOF

%nonassoc NOELSE 
%nonassoc ELSE 
%right ASSIGN CREATE 
%left OR 
%left AND 
%left EQ NEQ BLOCKEQ
%left LT GT LEQ GEQ 
%left PLUS MINUS 
%left TIMES DIVIDE 
%right NOT NEG

%start program 
%type <Ast.program> program

%%
