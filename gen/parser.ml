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
  | MOD
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
  | BUILD
  | JOIN
  | FRAME
  | SET
  | MAP
  | EOF
  | ID of (string)
  | STRING of (string)
  | FLOAT of (float)
  | LITERAL of (int)

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
  269 (* MOD *);
  270 (* FRAMEEQ *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GT *);
  276 (* GEQ *);
  277 (* TRUE *);
  278 (* FALSE *);
  279 (* AND *);
  280 (* OR *);
  281 (* DOT *);
  282 (* COLON *);
  283 (* RETURN *);
  284 (* IF *);
  285 (* ELSE *);
  286 (* FOR *);
  287 (* WHILE *);
  288 (* INT *);
  289 (* BOOL *);
  290 (* VOID *);
  291 (* BREAK *);
  292 (* CONTINUE *);
  293 (* BUILD *);
  294 (* JOIN *);
  295 (* FRAME *);
  296 (* SET *);
  297 (* MAP *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  298 (* ID *);
  299 (* STRING *);
  300 (* FLOAT *);
  301 (* LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\010\000\010\000\012\000\012\000\011\000\011\000\013\000\
\013\000\014\000\014\000\016\000\016\000\017\000\017\000\018\000\
\018\000\019\000\019\000\020\000\021\000\022\000\007\000\007\000\
\003\000\008\000\008\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\024\000\024\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\000\000\001\000\001\000\004\000\000\000\
\001\000\003\000\005\000\000\000\001\000\001\000\003\000\000\000\
\001\000\001\000\003\000\004\000\004\000\003\000\000\000\002\000\
\003\000\000\000\002\000\002\000\002\000\003\000\003\000\005\000\
\007\000\005\000\009\000\000\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\003\000\002\000\003\000\
\004\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\083\000\000\000\010\000\011\000\012\000\015\000\
\016\000\017\000\001\000\014\000\013\000\003\000\004\000\000\000\
\000\000\041\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\039\000\000\000\000\000\009\000\040\000\000\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\058\000\059\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\056\000\
\055\000\054\000\000\000\061\000\060\000\062\000\043\000\000\000\
\000\000\000\000\000\000\000\000\000\000\077\000\079\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\082\000\047\000\038\000\000\000\046\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\074\000\078\000\000\000\000\000\065\000\066\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\037\000\036\000\000\000\000\000\
\081\000\000\000\000\000\000\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\049\000\000\000\000\000\000\000\051\000"

let yydgoto = "\002\000\
\003\000\004\000\014\000\015\000\016\000\021\000\028\000\032\000\
\022\000\000\000\000\000\000\000\099\000\100\000\051\000\060\000\
\061\000\103\000\104\000\052\000\053\000\054\000\055\000\096\000"

let yysindex = "\255\255\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\224\254\
\077\255\000\000\159\001\233\254\024\255\052\255\000\000\064\255\
\159\001\000\000\033\255\159\001\000\000\000\000\041\255\072\255\
\096\255\006\000\006\000\000\000\006\000\006\000\000\000\000\000\
\021\255\107\255\109\255\134\255\084\255\146\255\108\255\000\000\
\000\000\000\000\239\000\000\000\000\000\000\000\000\000\006\000\
\041\001\116\255\135\001\126\255\149\255\000\000\000\000\000\000\
\003\001\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\121\255\124\255\000\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\000\000\000\000\000\000\006\000\000\000\059\001\135\001\166\255\
\077\001\167\255\171\255\163\255\095\001\135\001\174\255\172\255\
\135\001\000\000\000\000\144\255\144\255\000\000\000\000\078\255\
\132\255\132\255\220\255\220\255\220\255\220\255\170\001\153\001\
\135\001\204\255\006\000\204\255\000\000\000\000\006\000\006\000\
\000\000\006\000\150\255\023\001\000\000\115\001\135\001\135\001\
\204\255\006\000\006\000\000\000\177\255\135\001\204\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\180\255\000\000\000\000\181\255\000\000\000\000\
\000\000\000\000\000\000\250\255\000\000\000\000\000\000\000\000\
\000\000\000\000\099\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\184\255\
\000\000\000\000\025\255\000\000\054\255\000\000\000\000\000\000\
\000\000\000\000\191\255\000\000\192\255\024\255\194\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\122\255\000\000\
\000\000\000\000\000\000\195\255\000\000\001\255\000\000\196\255\
\015\255\000\000\000\000\075\000\099\000\000\000\000\000\123\000\
\066\255\170\255\147\000\171\000\195\000\219\000\214\255\154\000\
\129\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\160\255\000\000\000\000\000\000\009\255\067\255\
\000\000\206\255\000\000\000\000\000\000\102\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\179\000\000\000\245\255\000\000\000\000\175\000\
\000\000\000\000\000\000\000\000\000\000\000\000\224\255\142\000\
\000\000\000\000\000\000\000\000\000\000\000\000\145\255\076\000"

let yytablesize = 715
let yytable = "\001\000\
\011\000\057\000\059\000\034\000\062\000\063\000\034\000\020\000\
\065\000\017\000\131\000\026\000\133\000\027\000\026\000\080\000\
\031\000\080\000\023\000\080\000\080\000\064\000\034\000\059\000\
\056\000\140\000\024\000\030\000\037\000\030\000\030\000\144\000\
\038\000\094\000\095\000\097\000\059\000\101\000\102\000\105\000\
\080\000\039\000\040\000\108\000\109\000\110\000\111\000\112\000\
\113\000\114\000\115\000\116\000\117\000\118\000\119\000\120\000\
\029\000\025\000\029\000\121\000\045\000\046\000\047\000\048\000\
\049\000\050\000\068\000\026\000\068\000\035\000\068\000\068\000\
\035\000\034\000\029\000\035\000\036\000\018\000\019\000\037\000\
\068\000\068\000\033\000\038\000\076\000\077\000\078\000\079\000\
\068\000\068\000\132\000\068\000\039\000\040\000\134\000\135\000\
\018\000\136\000\041\000\042\000\069\000\043\000\044\000\028\000\
\027\000\095\000\142\000\027\000\066\000\071\000\067\000\045\000\
\046\000\047\000\048\000\049\000\050\000\034\000\072\000\035\000\
\090\000\073\000\053\000\037\000\053\000\042\000\042\000\038\000\
\042\000\042\000\091\000\031\000\074\000\031\000\031\000\068\000\
\039\000\040\000\076\000\077\000\078\000\079\000\041\000\042\000\
\080\000\043\000\044\000\070\000\083\000\084\000\085\000\086\000\
\078\000\079\000\092\000\045\000\046\000\047\000\048\000\049\000\
\050\000\048\000\106\000\048\000\048\000\107\000\123\000\048\000\
\127\000\125\000\069\000\048\000\069\000\126\000\069\000\069\000\
\129\000\130\000\137\000\143\000\048\000\048\000\006\000\007\000\
\069\000\069\000\048\000\048\000\028\000\048\000\048\000\052\000\
\069\000\069\000\028\000\069\000\032\000\025\000\033\000\048\000\
\048\000\048\000\048\000\048\000\048\000\034\000\030\000\035\000\
\052\000\058\000\098\000\037\000\000\000\141\000\075\000\038\000\
\075\000\000\000\075\000\075\000\000\000\000\000\000\000\000\000\
\039\000\040\000\076\000\077\000\078\000\079\000\041\000\042\000\
\080\000\043\000\044\000\000\000\075\000\075\000\000\000\075\000\
\000\000\000\000\000\000\045\000\046\000\047\000\048\000\049\000\
\050\000\000\000\000\000\042\000\000\000\042\000\042\000\000\000\
\000\000\042\000\000\000\000\000\000\000\042\000\000\000\034\000\
\000\000\056\000\000\000\000\000\000\000\037\000\042\000\042\000\
\000\000\038\000\000\000\000\000\042\000\042\000\000\000\042\000\
\042\000\000\000\039\000\040\000\000\000\000\000\000\000\000\000\
\005\000\006\000\007\000\042\000\000\000\000\000\042\000\008\000\
\009\000\010\000\000\000\012\000\013\000\045\000\046\000\047\000\
\048\000\049\000\050\000\057\000\000\000\057\000\000\000\057\000\
\057\000\057\000\057\000\057\000\057\000\000\000\000\000\057\000\
\000\000\057\000\057\000\057\000\057\000\057\000\057\000\000\000\
\000\000\057\000\057\000\063\000\057\000\063\000\000\000\063\000\
\063\000\063\000\063\000\000\000\000\000\000\000\000\000\063\000\
\000\000\063\000\063\000\063\000\063\000\063\000\063\000\000\000\
\000\000\063\000\063\000\064\000\063\000\064\000\000\000\064\000\
\064\000\064\000\064\000\000\000\000\000\000\000\000\000\064\000\
\000\000\064\000\064\000\064\000\064\000\064\000\064\000\000\000\
\000\000\064\000\064\000\067\000\064\000\067\000\000\000\067\000\
\067\000\000\000\000\000\000\000\000\000\000\000\000\000\067\000\
\000\000\067\000\067\000\067\000\067\000\067\000\067\000\000\000\
\000\000\067\000\067\000\070\000\067\000\070\000\000\000\070\000\
\070\000\000\000\076\000\000\000\076\000\000\000\076\000\076\000\
\000\000\070\000\070\000\070\000\070\000\070\000\070\000\000\000\
\000\000\070\000\070\000\071\000\070\000\071\000\000\000\071\000\
\071\000\076\000\000\000\076\000\000\000\000\000\000\000\000\000\
\000\000\071\000\071\000\071\000\071\000\071\000\071\000\000\000\
\000\000\071\000\071\000\072\000\071\000\072\000\000\000\072\000\
\072\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\000\072\000\072\000\072\000\072\000\072\000\000\000\
\000\000\072\000\072\000\073\000\072\000\073\000\000\000\073\000\
\073\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\073\000\073\000\073\000\073\000\073\000\073\000\075\000\
\000\000\073\000\073\000\000\000\073\000\076\000\077\000\078\000\
\079\000\000\000\000\000\080\000\000\000\081\000\082\000\083\000\
\084\000\085\000\086\000\093\000\000\000\087\000\088\000\000\000\
\000\000\076\000\077\000\078\000\079\000\000\000\000\000\080\000\
\000\000\081\000\082\000\083\000\084\000\085\000\086\000\138\000\
\000\000\087\000\088\000\000\000\000\000\076\000\077\000\078\000\
\079\000\000\000\000\000\080\000\000\000\081\000\082\000\083\000\
\084\000\085\000\086\000\089\000\000\000\087\000\088\000\076\000\
\077\000\078\000\079\000\000\000\000\000\080\000\000\000\081\000\
\082\000\083\000\084\000\085\000\086\000\122\000\000\000\087\000\
\088\000\076\000\077\000\078\000\079\000\000\000\000\000\080\000\
\000\000\081\000\082\000\083\000\084\000\085\000\086\000\124\000\
\000\000\087\000\088\000\076\000\077\000\078\000\079\000\000\000\
\000\000\080\000\000\000\081\000\082\000\083\000\084\000\085\000\
\086\000\000\000\000\000\087\000\088\000\076\000\077\000\078\000\
\079\000\000\000\000\000\080\000\000\000\081\000\082\000\083\000\
\084\000\085\000\086\000\000\000\000\000\087\000\088\000\000\000\
\128\000\076\000\077\000\078\000\079\000\000\000\000\000\080\000\
\000\000\081\000\082\000\083\000\084\000\085\000\086\000\000\000\
\000\000\087\000\088\000\000\000\139\000\076\000\077\000\078\000\
\079\000\000\000\000\000\080\000\000\000\081\000\082\000\083\000\
\084\000\085\000\086\000\000\000\000\000\087\000\088\000\076\000\
\077\000\078\000\079\000\000\000\000\000\080\000\000\000\081\000\
\082\000\083\000\084\000\085\000\086\000\000\000\000\000\087\000\
\076\000\077\000\078\000\079\000\000\000\000\000\080\000\000\000\
\081\000\082\000\083\000\084\000\085\000\086\000\005\000\006\000\
\007\000\000\000\000\000\000\000\000\000\008\000\009\000\010\000\
\000\000\012\000\013\000"

let yycheck = "\001\000\
\000\000\034\000\035\000\003\001\037\000\038\000\006\001\019\000\
\041\000\042\001\122\000\003\001\124\000\025\000\006\001\001\001\
\028\000\003\001\042\001\005\001\006\001\001\001\002\001\056\000\
\004\001\137\000\003\001\003\001\008\001\005\001\006\001\143\000\
\012\001\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\026\001\021\001\022\001\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\003\001\006\001\005\001\092\000\040\001\041\001\042\001\043\001\
\044\001\045\001\001\001\004\001\003\001\003\001\005\001\006\001\
\006\001\002\001\042\001\004\001\005\001\001\001\002\001\008\001\
\015\001\016\001\042\001\012\001\007\001\008\001\009\001\010\001\
\023\001\024\001\123\000\026\001\021\001\022\001\127\000\128\000\
\001\001\130\000\027\001\028\001\017\001\030\001\031\001\005\001\
\003\001\138\000\139\000\006\001\002\001\002\001\002\001\040\001\
\041\001\042\001\043\001\044\001\045\001\002\001\011\001\004\001\
\005\001\014\001\001\001\008\001\003\001\027\001\028\001\012\001\
\030\001\031\001\005\001\003\001\025\001\005\001\006\001\002\001\
\021\001\022\001\007\001\008\001\009\001\010\001\027\001\028\001\
\013\001\030\001\031\001\002\001\017\001\018\001\019\001\020\001\
\009\001\010\001\006\001\040\001\041\001\042\001\043\001\044\001\
\045\001\002\001\042\001\004\001\005\001\042\001\001\001\008\001\
\006\001\003\001\001\001\012\001\003\001\003\001\005\001\006\001\
\003\001\006\001\029\001\003\001\021\001\022\001\003\001\003\001\
\015\001\016\001\027\001\028\001\005\001\030\001\031\001\001\001\
\023\001\024\001\003\001\026\001\003\001\003\001\003\001\040\001\
\041\001\042\001\043\001\044\001\045\001\002\001\028\000\004\001\
\003\001\035\000\069\000\008\001\255\255\138\000\001\001\012\001\
\003\001\255\255\005\001\006\001\255\255\255\255\255\255\255\255\
\021\001\022\001\007\001\008\001\009\001\010\001\027\001\028\001\
\013\001\030\001\031\001\255\255\023\001\024\001\255\255\026\001\
\255\255\255\255\255\255\040\001\041\001\042\001\043\001\044\001\
\045\001\255\255\255\255\002\001\255\255\004\001\005\001\255\255\
\255\255\008\001\255\255\255\255\255\255\012\001\255\255\002\001\
\255\255\004\001\255\255\255\255\255\255\008\001\021\001\022\001\
\255\255\012\001\255\255\255\255\027\001\028\001\255\255\030\001\
\031\001\255\255\021\001\022\001\255\255\255\255\255\255\255\255\
\032\001\033\001\034\001\042\001\255\255\255\255\045\001\039\001\
\040\001\041\001\255\255\043\001\044\001\040\001\041\001\042\001\
\043\001\044\001\045\001\001\001\255\255\003\001\255\255\005\001\
\006\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\026\001\003\001\255\255\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\026\001\003\001\255\255\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\026\001\003\001\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\026\001\003\001\255\255\005\001\
\006\001\255\255\001\001\255\255\003\001\255\255\005\001\006\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\026\001\003\001\255\255\005\001\
\006\001\024\001\255\255\026\001\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\026\001\003\001\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\001\001\026\001\003\001\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\255\255\023\001\024\001\255\255\026\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\255\255\023\001\024\001\255\255\
\255\255\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\255\255\023\001\024\001\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\003\001\255\255\023\001\024\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\003\001\255\255\023\001\
\024\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\003\001\
\255\255\023\001\024\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\023\001\024\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\023\001\024\001\255\255\
\026\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\023\001\024\001\255\255\026\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\023\001\024\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\032\001\033\001\
\034\001\255\255\255\255\255\255\255\255\039\001\040\001\041\001\
\255\255\043\001\044\001"

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
  MOD\000\
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
  BUILD\000\
  JOIN\000\
  FRAME\000\
  SET\000\
  MAP\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  STRING\000\
  FLOAT\000\
  LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 39 "src/parser.mly"
            ( _1 )
# 447 "src/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "src/parser.mly"
                 ( [], []                 )
# 453 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 43 "src/parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 461 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 44 "src/parser.mly"
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
# 48 "src/parser.mly"
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
# 58 "src/parser.mly"
                  ( []          )
# 493 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 59 "src/parser.mly"
                  ( List.rev _1 )
# 500 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "src/parser.mly"
                             ( [(_1, _2)]     )
# 508 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "src/parser.mly"
                             ( (_3, _4) :: _1 )
# 517 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "src/parser.mly"
          ( Int    )
# 523 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "src/parser.mly"
          ( Bool   )
# 529 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "src/parser.mly"
          ( Void   )
# 535 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 69 "src/parser.mly"
          ( Float  )
# 542 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "src/parser.mly"
          ( String )
# 549 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "src/parser.mly"
          ( Frame  )
# 555 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "src/parser.mly"
          ( Set    )
# 561 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "src/parser.mly"
          ( Map    )
# 567 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 76 "src/parser.mly"
            ([_1])
# 574 "src/parser.ml"
               : 'typedef_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typedef_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 77 "src/parser.mly"
                                 (_3::_1)
# 582 "src/parser.ml"
               : 'typedef_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "src/parser.mly"
                 ([])
# 588 "src/parser.ml"
               : 'typedef_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typedef_list) in
    Obj.repr(
# 81 "src/parser.mly"
                   (List.rev _1)
# 595 "src/parser.ml"
               : 'typedef_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "src/parser.mly"
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
# 611 "src/parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typedef_list_opt) in
    Obj.repr(
# 95 "src/parser.mly"
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
# 637 "src/parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "src/parser.mly"
                ([])
# 643 "src/parser.ml"
               : 'expr_pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_pair_true_list) in
    Obj.repr(
# 118 "src/parser.mly"
                          (_1)
# 650 "src/parser.ml"
               : 'expr_pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "src/parser.mly"
                      ([(_1, _3)])
# 658 "src/parser.ml"
               : 'expr_pair_true_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expr_pair_true_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "src/parser.mly"
                                                ((_3,_5)::_1)
# 667 "src/parser.ml"
               : 'expr_pair_true_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "src/parser.mly"
                        ( [] )
# 673 "src/parser.ml"
               : 'expr_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_true_list_set) in
    Obj.repr(
# 126 "src/parser.mly"
                        ( _1 )
# 680 "src/parser.ml"
               : 'expr_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "src/parser.mly"
                            ( [_1]     )
# 687 "src/parser.ml"
               : 'expr_true_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_true_list_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "src/parser.mly"
                                  ( _3 :: _1 )
# 695 "src/parser.ml"
               : 'expr_true_list_set))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "src/parser.mly"
                  ( []          )
# 701 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 135 "src/parser.mly"
                  ( List.rev _1 )
# 708 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "src/parser.mly"
                            ( [_1]     )
# 715 "src/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "src/parser.mly"
                            ( _3 :: _1 )
# 723 "src/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_pair_list) in
    Obj.repr(
# 142 "src/parser.mly"
                                     (Map(List.rev _3))
# 730 "src/parser.ml"
               : 'map))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list_set) in
    Obj.repr(
# 144 "src/parser.mly"
                                (Set(List.rev _3))
# 737 "src/parser.ml"
               : 'set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list_set) in
    Obj.repr(
# 146 "src/parser.mly"
                                (Array (List.rev _2))
# 744 "src/parser.ml"
               : 'arr))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "src/parser.mly"
                     ( []       )
# 750 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 150 "src/parser.mly"
                     ( _2 :: _1 )
# 758 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 153 "src/parser.mly"
               ( (_1, _2) )
# 766 "src/parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "src/parser.mly"
                   ( []       )
# 772 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 157 "src/parser.mly"
                   ( _2 :: _1 )
# 780 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 160 "src/parser.mly"
                                            ( Expr _1               )
# 787 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "src/parser.mly"
                                            ( Return Noexpr         )
# 793 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 162 "src/parser.mly"
                                            ( Return _2             )
# 800 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 163 "src/parser.mly"
                                            ( Block(List.rev _2)    )
# 807 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 164 "src/parser.mly"
                                            ( If(_3, _5, Block([])) )
# 815 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 165 "src/parser.mly"
                                            ( If(_3, _5, _7)        )
# 824 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 166 "src/parser.mly"
                                            ( While(_3, _5)         )
# 832 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 168 "src/parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 842 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 171 "src/parser.mly"
                  ( Noexpr )
# 848 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "src/parser.mly"
                  ( _1     )
# 855 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 175 "src/parser.mly"
                                 ( Literal(_1)            )
# 862 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 176 "src/parser.mly"
                                 ( Float(_1)              )
# 869 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 177 "src/parser.mly"
                                 ( String(_1)             )
# 876 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 178 "src/parser.mly"
                                 ( Id(_1)                 )
# 883 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "src/parser.mly"
                                 ( BoolLit(true)          )
# 889 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "src/parser.mly"
                                 ( BoolLit(false)         )
# 895 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set) in
    Obj.repr(
# 181 "src/parser.mly"
                                 ( _1                     )
# 902 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'map) in
    Obj.repr(
# 182 "src/parser.mly"
                                 ( _1                     )
# 909 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arr) in
    Obj.repr(
# 183 "src/parser.mly"
                                 ( _1                     )
# 916 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "src/parser.mly"
                                 ( Binop(_1, Add,     _3) )
# 924 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "src/parser.mly"
                                 ( Binop(_1, Sub,     _3) )
# 932 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "src/parser.mly"
                                 ( Binop(_1, Mult,    _3) )
# 940 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "src/parser.mly"
                                 ( Binop(_1, Div,     _3) )
# 948 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "src/parser.mly"
                                 ( Binop(_1, Mod,     _3) )
# 956 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "src/parser.mly"
                                 ( Binop(_1, Equal,   _3) )
# 964 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "src/parser.mly"
                                 ( Binop(_1, Neq,     _3) )
# 972 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "src/parser.mly"
                                 ( Binop(_1, Less,    _3) )
# 980 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "src/parser.mly"
                                 ( Binop(_1, Leq,     _3) )
# 988 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "src/parser.mly"
                                 ( Binop(_1, Greater, _3) )
# 996 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "src/parser.mly"
                                 ( Binop(_1, Geq,     _3) )
# 1004 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 195 "src/parser.mly"
                                 ( Binop(_1, FrameEq, _3) )
# 1012 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "src/parser.mly"
                                 ( Binop(_1, And,     _3) )
# 1020 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 197 "src/parser.mly"
                                 ( Binop(_1, Or,      _3) )
# 1028 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "src/parser.mly"
                                 ( Unop(Neg, _2)          )
# 1035 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 199 "src/parser.mly"
                                 ( Objid(_1, _3)          )
# 1043 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "src/parser.mly"
                                 ( Unop(Not, _2)          )
# 1050 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "src/parser.mly"
                                 ( Assign(_1, _3)         )
# 1058 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 202 "src/parser.mly"
                                 ( Call(_1, _3)           )
# 1066 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 203 "src/parser.mly"
                                 ( _2                     )
# 1073 "src/parser.ml"
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
