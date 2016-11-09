type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | FRAMEEQ
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | DOT
  | COLON
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | BREAK
  | CONTINUE
  | CREATE
  | BUILD
  | JOIN
  | FRAME
  | SET
  | MAP
  | EOF
  | ID of (string)
  | STRINGLIT of (string)
  | FLOATLIT of (float)
  | INTLIT of (int)

open Parsing;;
let _ = parse_error;;
# 3 "src/parser.mly"
 open Ast 
# 54 "src/parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* NOT *);
  269 (* FRAMEEQ *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LT *);
  273 (* LEQ *);
  274 (* GT *);
  275 (* GEQ *);
  276 (* TRUE *);
  277 (* FALSE *);
  278 (* AND *);
  279 (* OR *);
  280 (* DOT *);
  281 (* COLON *);
  282 (* RETURN *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* FOR *);
  286 (* WHILE *);
  287 (* INT *);
  288 (* BOOL *);
  289 (* VOID *);
  290 (* BREAK *);
  291 (* CONTINUE *);
  292 (* CREATE *);
  293 (* BUILD *);
  294 (* JOIN *);
  295 (* FRAME *);
  296 (* SET *);
  297 (* MAP *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  298 (* ID *);
  299 (* STRINGLIT *);
  300 (* FLOATLIT *);
  301 (* INTLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\010\000\
\010\000\012\000\012\000\011\000\011\000\013\000\013\000\014\000\
\014\000\016\000\016\000\017\000\017\000\018\000\018\000\019\000\
\019\000\020\000\021\000\022\000\007\000\007\000\003\000\008\000\
\008\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\024\000\024\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\000\000\001\000\001\000\004\000\000\000\001\000\003\000\
\005\000\000\000\001\000\001\000\003\000\000\000\001\000\001\000\
\003\000\004\000\004\000\003\000\000\000\002\000\003\000\000\000\
\002\000\002\000\002\000\003\000\003\000\005\000\007\000\005\000\
\009\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\002\000\003\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\080\000\000\000\010\000\011\000\012\000\015\000\
\013\000\014\000\001\000\003\000\004\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\037\000\
\000\000\000\000\009\000\038\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\054\000\055\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\000\052\000\000\000\
\058\000\057\000\059\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\074\000\076\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\079\000\045\000\036\000\
\000\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\075\000\000\000\
\000\000\062\000\063\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\034\000\000\000\000\000\078\000\000\000\000\000\
\000\000\048\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\000\000\000\000\000\000\049\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\019\000\026\000\030\000\
\020\000\000\000\000\000\000\000\096\000\097\000\048\000\057\000\
\058\000\100\000\101\000\049\000\050\000\051\000\052\000\093\000"

let yysindex = "\030\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\255\014\255\000\000\
\254\255\025\255\072\255\076\255\000\000\100\255\254\255\000\000\
\085\255\254\255\000\000\000\000\091\255\092\000\138\255\236\000\
\236\000\000\000\236\000\236\000\000\000\000\000\066\000\157\255\
\158\255\167\255\134\255\180\255\074\255\000\000\000\000\148\255\
\000\000\000\000\000\000\000\000\236\000\237\255\123\000\060\001\
\178\255\186\255\000\000\000\000\000\000\171\255\236\000\236\000\
\236\000\236\000\236\000\236\000\236\000\154\255\000\000\236\000\
\236\000\236\000\236\000\236\000\236\000\236\000\236\000\236\000\
\236\000\236\000\236\000\236\000\236\000\000\000\000\000\000\000\
\236\000\000\000\002\000\060\001\200\255\043\000\199\255\207\255\
\209\255\019\001\060\001\213\255\214\255\060\001\000\000\031\255\
\031\255\000\000\000\000\010\255\010\255\010\255\079\255\079\255\
\079\255\079\255\095\001\079\001\108\255\060\001\185\000\236\000\
\185\000\000\000\000\000\236\000\236\000\000\000\236\000\196\255\
\190\255\000\000\038\001\060\001\060\001\185\000\236\000\236\000\
\000\000\224\255\060\001\185\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\225\255\000\000\000\000\233\255\000\000\000\000\000\000\000\000\
\000\000\216\000\000\000\000\000\000\000\000\000\000\000\000\000\
\251\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\055\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\238\255\000\000\000\000\135\255\
\000\000\105\255\000\000\000\000\000\000\000\000\000\000\241\255\
\000\000\245\255\072\255\246\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\172\255\000\000\000\000\000\000\000\000\
\255\255\000\000\001\255\000\000\003\000\146\001\000\000\106\255\
\129\255\000\000\000\000\078\255\070\000\186\001\249\000\114\001\
\140\001\163\001\076\000\007\255\216\255\220\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\154\000\
\000\000\000\000\000\000\008\255\093\255\000\000\005\000\000\000\
\000\000\000\000\208\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\231\000\000\000\151\000\000\000\000\000\237\000\
\000\000\000\000\000\000\000\000\000\000\000\000\226\255\203\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\001\143\000"

let yytablesize = 723
let yytable = "\026\000\
\011\000\054\000\056\000\032\000\059\000\060\000\032\000\073\000\
\062\000\073\000\024\000\073\000\073\000\024\000\016\000\017\000\
\072\000\073\000\074\000\075\000\040\000\040\000\056\000\040\000\
\040\000\079\000\080\000\081\000\082\000\073\000\001\000\073\000\
\091\000\092\000\094\000\056\000\098\000\099\000\102\000\074\000\
\075\000\104\000\105\000\106\000\107\000\108\000\109\000\110\000\
\111\000\112\000\113\000\114\000\115\000\116\000\117\000\056\000\
\085\000\056\000\118\000\056\000\056\000\056\000\056\000\056\000\
\056\000\015\000\021\000\056\000\056\000\056\000\056\000\056\000\
\056\000\056\000\022\000\068\000\056\000\056\000\071\000\056\000\
\071\000\023\000\071\000\071\000\069\000\072\000\073\000\074\000\
\075\000\129\000\071\000\071\000\071\000\131\000\132\000\033\000\
\133\000\070\000\033\000\071\000\071\000\056\000\071\000\024\000\
\092\000\139\000\060\000\027\000\060\000\027\000\060\000\060\000\
\060\000\060\000\072\000\073\000\074\000\075\000\060\000\060\000\
\060\000\060\000\060\000\060\000\060\000\085\000\027\000\060\000\
\060\000\061\000\060\000\061\000\031\000\061\000\061\000\061\000\
\061\000\028\000\016\000\028\000\028\000\061\000\061\000\061\000\
\061\000\061\000\061\000\061\000\071\000\066\000\061\000\061\000\
\060\000\061\000\072\000\073\000\074\000\075\000\063\000\064\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\018\000\
\065\000\083\000\084\000\090\000\051\000\025\000\051\000\061\000\
\029\000\072\000\073\000\074\000\075\000\067\000\088\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\135\000\089\000\
\083\000\084\000\085\000\103\000\072\000\073\000\074\000\075\000\
\120\000\122\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\123\000\025\000\083\000\084\000\025\000\124\000\126\000\
\064\000\085\000\064\000\127\000\064\000\064\000\029\000\134\000\
\029\000\029\000\140\000\006\000\064\000\064\000\064\000\064\000\
\064\000\064\000\064\000\007\000\085\000\064\000\064\000\086\000\
\064\000\050\000\026\000\072\000\073\000\074\000\075\000\026\000\
\030\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\028\000\023\000\083\000\084\000\119\000\031\000\064\000\050\000\
\072\000\073\000\074\000\075\000\095\000\055\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\138\000\000\000\083\000\
\084\000\000\000\000\000\085\000\005\000\006\000\007\000\005\000\
\006\000\007\000\000\000\000\000\008\000\009\000\010\000\008\000\
\009\000\010\000\000\000\000\000\000\000\121\000\000\000\000\000\
\085\000\072\000\073\000\074\000\075\000\000\000\000\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\000\000\000\000\
\083\000\084\000\061\000\032\000\000\000\053\000\065\000\000\000\
\065\000\035\000\065\000\065\000\072\000\036\000\072\000\000\000\
\072\000\072\000\065\000\065\000\065\000\037\000\038\000\000\000\
\000\000\085\000\000\000\065\000\065\000\032\000\065\000\033\000\
\034\000\072\000\072\000\035\000\072\000\000\000\000\000\036\000\
\000\000\043\000\044\000\045\000\000\000\046\000\047\000\037\000\
\038\000\000\000\000\000\000\000\000\000\039\000\040\000\000\000\
\041\000\042\000\000\000\128\000\032\000\130\000\033\000\087\000\
\000\000\000\000\035\000\043\000\044\000\045\000\036\000\046\000\
\047\000\000\000\137\000\000\000\000\000\000\000\037\000\038\000\
\141\000\000\000\000\000\000\000\039\000\040\000\000\000\041\000\
\042\000\000\000\000\000\046\000\000\000\046\000\046\000\000\000\
\000\000\046\000\043\000\044\000\045\000\046\000\046\000\047\000\
\000\000\000\000\000\000\000\000\000\000\046\000\046\000\000\000\
\000\000\000\000\000\000\046\000\046\000\000\000\046\000\046\000\
\000\000\000\000\032\000\000\000\033\000\000\000\000\000\000\000\
\035\000\046\000\046\000\046\000\036\000\046\000\046\000\000\000\
\000\000\000\000\000\000\000\000\037\000\038\000\000\000\000\000\
\000\000\000\000\039\000\040\000\000\000\041\000\042\000\000\000\
\000\000\040\000\000\000\040\000\040\000\000\000\000\000\040\000\
\043\000\044\000\045\000\040\000\046\000\047\000\000\000\000\000\
\000\000\000\000\000\000\040\000\040\000\032\000\000\000\053\000\
\000\000\040\000\040\000\035\000\040\000\040\000\000\000\036\000\
\000\000\067\000\000\000\067\000\000\000\067\000\067\000\037\000\
\038\000\040\000\000\000\040\000\040\000\067\000\067\000\067\000\
\067\000\067\000\067\000\067\000\000\000\000\000\067\000\067\000\
\000\000\067\000\000\000\043\000\044\000\045\000\000\000\046\000\
\047\000\072\000\073\000\074\000\075\000\000\000\000\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\000\000\000\000\
\083\000\084\000\000\000\125\000\072\000\073\000\074\000\075\000\
\000\000\000\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\000\000\000\000\083\000\084\000\000\000\136\000\000\000\
\000\000\085\000\072\000\073\000\074\000\075\000\000\000\000\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\000\000\
\000\000\083\000\084\000\000\000\085\000\072\000\073\000\074\000\
\075\000\000\000\000\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\000\000\000\000\083\000\072\000\073\000\074\000\
\075\000\000\000\085\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\068\000\000\000\068\000\000\000\068\000\068\000\
\000\000\000\000\000\000\000\000\000\000\085\000\068\000\068\000\
\068\000\068\000\068\000\068\000\068\000\000\000\000\000\068\000\
\068\000\000\000\068\000\000\000\069\000\085\000\069\000\000\000\
\069\000\069\000\077\000\000\000\077\000\000\000\077\000\077\000\
\069\000\069\000\069\000\069\000\069\000\069\000\069\000\000\000\
\000\000\069\000\069\000\070\000\069\000\070\000\000\000\070\000\
\070\000\000\000\077\000\000\000\000\000\000\000\000\000\070\000\
\070\000\070\000\070\000\070\000\070\000\070\000\000\000\000\000\
\070\000\070\000\066\000\070\000\066\000\000\000\066\000\066\000\
\000\000\000\000\000\000\000\000\000\000\000\000\066\000\066\000\
\066\000\000\000\000\000\000\000\000\000\000\000\000\000\066\000\
\066\000\000\000\066\000"

let yycheck = "\005\001\
\000\000\032\000\033\000\003\001\035\000\036\000\006\001\001\001\
\039\000\003\001\003\001\005\001\006\001\006\001\001\001\002\001\
\007\001\008\001\009\001\010\001\026\001\027\001\053\000\029\001\
\030\001\016\001\017\001\018\001\019\001\023\001\001\000\025\001\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\009\001\
\010\001\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\001\001\
\047\001\003\001\089\000\005\001\006\001\007\001\008\001\009\001\
\010\001\042\001\042\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\003\001\002\001\022\001\023\001\001\001\025\001\
\003\001\006\001\005\001\006\001\011\001\007\001\008\001\009\001\
\010\001\120\000\013\001\014\001\015\001\124\000\125\000\003\001\
\127\000\024\001\006\001\022\001\023\001\047\001\025\001\004\001\
\135\000\136\000\001\001\003\001\003\001\005\001\005\001\006\001\
\007\001\008\001\007\001\008\001\009\001\010\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\047\001\042\001\022\001\
\023\001\001\001\025\001\003\001\042\001\005\001\006\001\007\001\
\008\001\003\001\001\001\005\001\006\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\001\001\016\001\022\001\023\001\
\047\001\025\001\007\001\008\001\009\001\010\001\002\001\002\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\017\000\
\002\001\022\001\023\001\001\001\001\001\023\000\003\001\047\001\
\026\000\007\001\008\001\009\001\010\001\002\001\005\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\001\001\006\001\
\022\001\023\001\047\001\042\001\007\001\008\001\009\001\010\001\
\001\001\003\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\003\001\003\001\022\001\023\001\006\001\006\001\003\001\
\001\001\047\001\003\001\006\001\005\001\006\001\003\001\028\001\
\005\001\006\001\003\001\003\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\003\001\047\001\022\001\023\001\003\001\
\025\001\001\001\005\001\007\001\008\001\009\001\010\001\003\001\
\003\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\026\000\003\001\022\001\023\001\003\001\003\001\047\001\003\001\
\007\001\008\001\009\001\010\001\066\000\033\000\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\135\000\255\255\022\001\
\023\001\255\255\255\255\047\001\031\001\032\001\033\001\031\001\
\032\001\033\001\255\255\255\255\039\001\040\001\041\001\039\001\
\040\001\041\001\255\255\255\255\255\255\003\001\255\255\255\255\
\047\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\022\001\023\001\001\001\002\001\255\255\004\001\001\001\255\255\
\003\001\008\001\005\001\006\001\001\001\012\001\003\001\255\255\
\005\001\006\001\013\001\014\001\015\001\020\001\021\001\255\255\
\255\255\047\001\255\255\022\001\023\001\002\001\025\001\004\001\
\005\001\022\001\023\001\008\001\025\001\255\255\255\255\012\001\
\255\255\040\001\041\001\042\001\255\255\044\001\045\001\020\001\
\021\001\255\255\255\255\255\255\255\255\026\001\027\001\255\255\
\029\001\030\001\255\255\119\000\002\001\121\000\004\001\005\001\
\255\255\255\255\008\001\040\001\041\001\042\001\012\001\044\001\
\045\001\255\255\134\000\255\255\255\255\255\255\020\001\021\001\
\140\000\255\255\255\255\255\255\026\001\027\001\255\255\029\001\
\030\001\255\255\255\255\002\001\255\255\004\001\005\001\255\255\
\255\255\008\001\040\001\041\001\042\001\012\001\044\001\045\001\
\255\255\255\255\255\255\255\255\255\255\020\001\021\001\255\255\
\255\255\255\255\255\255\026\001\027\001\255\255\029\001\030\001\
\255\255\255\255\002\001\255\255\004\001\255\255\255\255\255\255\
\008\001\040\001\041\001\042\001\012\001\044\001\045\001\255\255\
\255\255\255\255\255\255\255\255\020\001\021\001\255\255\255\255\
\255\255\255\255\026\001\027\001\255\255\029\001\030\001\255\255\
\255\255\002\001\255\255\004\001\005\001\255\255\255\255\008\001\
\040\001\041\001\042\001\012\001\044\001\045\001\255\255\255\255\
\255\255\255\255\255\255\020\001\021\001\002\001\255\255\004\001\
\255\255\026\001\027\001\008\001\029\001\030\001\255\255\012\001\
\255\255\001\001\255\255\003\001\255\255\005\001\006\001\020\001\
\021\001\042\001\255\255\044\001\045\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\255\255\255\255\022\001\023\001\
\255\255\025\001\255\255\040\001\041\001\042\001\255\255\044\001\
\045\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\022\001\023\001\255\255\025\001\007\001\008\001\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\255\255\255\255\022\001\023\001\255\255\025\001\255\255\
\255\255\047\001\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\255\255\
\255\255\022\001\023\001\255\255\047\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\255\255\255\255\022\001\007\001\008\001\009\001\
\010\001\255\255\047\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\001\001\255\255\003\001\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\047\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\255\255\255\255\022\001\
\023\001\255\255\025\001\255\255\001\001\047\001\003\001\255\255\
\005\001\006\001\001\001\255\255\003\001\255\255\005\001\006\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\255\255\
\255\255\022\001\023\001\001\001\025\001\003\001\255\255\005\001\
\006\001\255\255\025\001\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\255\255\
\022\001\023\001\001\001\025\001\003\001\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\255\255\025\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  FRAMEEQ\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  DOT\000\
  COLON\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  VOID\000\
  BREAK\000\
  CONTINUE\000\
  CREATE\000\
  BUILD\000\
  JOIN\000\
  FRAME\000\
  SET\000\
  MAP\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  STRINGLIT\000\
  FLOATLIT\000\
  INTLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 40 "src/parser.mly"
            ( _1 )
# 447 "src/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "src/parser.mly"
                 ( [], []                 )
# 453 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 44 "src/parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 461 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 45 "src/parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 469 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 49 "src/parser.mly"
    (
      { typ     = _1;
        fname   = _2;
        formals = _4;
        locals  = List.rev _7;
        body    = List.rev _8
      }
    )
# 487 "src/parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "src/parser.mly"
                  ( []          )
# 493 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 60 "src/parser.mly"
                  ( List.rev _1 )
# 500 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "src/parser.mly"
                             ( [(_1, _2)]     )
# 508 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "src/parser.mly"
                             ( (_3, _4) :: _1 )
# 517 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "src/parser.mly"
          ( Int   )
# 523 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "src/parser.mly"
          ( Bool  )
# 529 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "src/parser.mly"
          ( Void  )
# 535 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "src/parser.mly"
          ( Set   )
# 541 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "src/parser.mly"
          ( Map   )
# 547 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "src/parser.mly"
          ( Frame )
# 553 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 75 "src/parser.mly"
            ([_1])
# 560 "src/parser.ml"
               : 'typedef_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typedef_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 76 "src/parser.mly"
                                 (_3::_1)
# 568 "src/parser.ml"
               : 'typedef_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "src/parser.mly"
                 ([])
# 574 "src/parser.ml"
               : 'typedef_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typedef_list) in
    Obj.repr(
# 80 "src/parser.mly"
                   (List.rev _1)
# 581 "src/parser.ml"
               : 'typedef_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "src/parser.mly"
       (
        match _1 with
        | "Int" -> Int
        | "Bool" -> Bool
        | "Void" -> Void
        | "String" -> String
        | "Float" -> Float
        | "Map" | "Set" -> failwith ("set map init must with parameters")
        | x -> Class x
    )
# 597 "src/parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typedef_list_opt) in
    Obj.repr(
# 94 "src/parser.mly"
                                (
        match _1 with
        | "Set" -> begin
                match _3 with
                |[x] -> Set x
                | _ -> failwith ("set just with one parameter")
                end
        | "Map" -> begin
                match _3 with
                | [x;y] -> Map (x,y)
                | _ -> failwith ("map just two parameter")
                end
        | "Array" -> begin
               match _3 with
               |[x] -> Array x
               | _ -> failwith ("array just with one parameter")
               end
        | _ -> failwith ("not suppport template except set map")
    )
# 623 "src/parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "src/parser.mly"
                ([])
# 629 "src/parser.ml"
               : 'expr_pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_pair_true_list) in
    Obj.repr(
# 120 "src/parser.mly"
                          (_1)
# 636 "src/parser.ml"
               : 'expr_pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "src/parser.mly"
                      ([(_1, _3)])
# 644 "src/parser.ml"
               : 'expr_pair_true_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expr_pair_true_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "src/parser.mly"
                                                ((_3,_5)::_1)
# 653 "src/parser.ml"
               : 'expr_pair_true_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "src/parser.mly"
                        ( [] )
# 659 "src/parser.ml"
               : 'expr_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_true_list_set) in
    Obj.repr(
# 129 "src/parser.mly"
                        ( _1 )
# 666 "src/parser.ml"
               : 'expr_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "src/parser.mly"
                            ( [_1]     )
# 673 "src/parser.ml"
               : 'expr_true_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_true_list_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "src/parser.mly"
                                  ( _3 :: _1 )
# 681 "src/parser.ml"
               : 'expr_true_list_set))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "src/parser.mly"
                  ( []          )
# 687 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 139 "src/parser.mly"
                  ( List.rev _1 )
# 694 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "src/parser.mly"
                            ( [_1]     )
# 701 "src/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "src/parser.mly"
                            ( _3 :: _1 )
# 709 "src/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_pair_list) in
    Obj.repr(
# 147 "src/parser.mly"
                                     (Map(List.rev _3))
# 716 "src/parser.ml"
               : 'map))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list_set) in
    Obj.repr(
# 149 "src/parser.mly"
                                (Set(List.rev _3))
# 723 "src/parser.ml"
               : 'set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list_set) in
    Obj.repr(
# 151 "src/parser.mly"
                                (Array (List.rev _2))
# 730 "src/parser.ml"
               : 'arr))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "src/parser.mly"
                     ( []       )
# 736 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 159 "src/parser.mly"
                     ( _2 :: _1 )
# 744 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 162 "src/parser.mly"
               ( (_1, _2) )
# 752 "src/parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "src/parser.mly"
                   ( []       )
# 758 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 166 "src/parser.mly"
                   ( _2 :: _1 )
# 766 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 169 "src/parser.mly"
                                            ( Expr _1               )
# 773 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "src/parser.mly"
                                            ( Return Noexpr         )
# 779 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 171 "src/parser.mly"
                                            ( Return _2             )
# 786 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 172 "src/parser.mly"
                                            ( Block(List.rev _2)    )
# 793 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 173 "src/parser.mly"
                                            ( If(_3, _5, Block([])) )
# 801 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 174 "src/parser.mly"
                                            ( If(_3, _5, _7)        )
# 810 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 175 "src/parser.mly"
                                            ( While(_3, _5)         )
# 818 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 177 "src/parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 828 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "src/parser.mly"
                  ( Noexpr )
# 834 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 181 "src/parser.mly"
                  ( _1     )
# 841 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 184 "src/parser.mly"
                                 ( IntLit(_1)            )
# 848 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 185 "src/parser.mly"
                                 ( FloatLit($))
# 855 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "src/parser.mly"
                                 ( BoolLit(true)          )
# 861 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "src/parser.mly"
                                 ( BoolLit(false)         )
# 867 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 188 "src/parser.mly"
                                 ( Id(_1)                 )
# 874 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set) in
    Obj.repr(
# 189 "src/parser.mly"
                                 ( _1                     )
# 881 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'map) in
    Obj.repr(
# 190 "src/parser.mly"
                                 ( _1                     )
# 888 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arr) in
    Obj.repr(
# 191 "src/parser.mly"
                                 ( _1                     )
# 895 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "src/parser.mly"
                                 ( Binop(_1, Add,     _3) )
# 903 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "src/parser.mly"
                                 ( Binop(_1, Sub,     _3) )
# 911 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "src/parser.mly"
                                 ( Binop(_1, Mult,    _3) )
# 919 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "src/parser.mly"
                                 ( Binop(_1, Div,     _3) )
# 927 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "src/parser.mly"
                                 ( Binop(_1, Mod,     _3) )
# 935 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "src/parser.mly"
                                 ( Binop(_1, Equal,   _3) )
# 943 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "src/parser.mly"
                                 ( Binop(_1, Neq,     _3) )
# 951 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 199 "src/parser.mly"
                                 ( Binop(_1, Less,    _3) )
# 959 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "src/parser.mly"
                                 ( Binop(_1, Leq,     _3) )
# 967 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "src/parser.mly"
                                 ( Binop(_1, Greater, _3) )
# 975 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 202 "src/parser.mly"
                                 ( Binop(_1, Geq,     _3) )
# 983 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 203 "src/parser.mly"
                                 ( Binop(_1, FrameEq, _3) )
# 991 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 204 "src/parser.mly"
                                 ( Binop(_1, And,     _3) )
# 999 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 205 "src/parser.mly"
                                 ( Binop(_1, Or,      _3) )
# 1007 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 206 "src/parser.mly"
                                 ( Unop(Neg, _2)          )
# 1014 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 207 "src/parser.mly"
                                 ( Objid(_1, _3)          )
# 1022 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 208 "src/parser.mly"
                                 ( Unop(Not, _2)          )
# 1029 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 209 "src/parser.mly"
                                 ( Assign(_1, _3)         )
# 1037 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 210 "src/parser.mly"
                                 ( Call(_1, _3)           )
# 1045 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 211 "src/parser.mly"
                                 ( _2                     )
# 1052 "src/parser.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
