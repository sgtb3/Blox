type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
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
  | IF
  | ELSE
  | FOR
  | INT
  | BOOL
  | VOID
  | BUILD
  | JOIN
  | FRAME
  | SET
  | EOF
  | LTN
  | GTN
  | ID of (string)
  | STRING of (string)
  | FLOAT of (float)
  | LITERAL of (int)

open Parsing;;
let _ = parse_error;;
# 3 "src/parser.mly"
 open Ast 
# 51 "src/parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* NOT *);
  271 (* FRAMEEQ *);
  272 (* EQ *);
  273 (* NEQ *);
  274 (* LT *);
  275 (* LEQ *);
  276 (* GT *);
  277 (* GEQ *);
  278 (* TRUE *);
  279 (* FALSE *);
  280 (* AND *);
  281 (* OR *);
  282 (* DOT *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* FOR *);
  286 (* INT *);
  287 (* BOOL *);
  288 (* VOID *);
  289 (* BUILD *);
  290 (* JOIN *);
  291 (* FRAME *);
  292 (* SET *);
    0 (* EOF *);
  293 (* LTN *);
  294 (* GTN *);
    0|]

let yytransl_block = [|
  295 (* ID *);
  296 (* STRING *);
  297 (* FLOAT *);
  298 (* LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\005\000\006\000\
\006\000\006\000\007\000\007\000\009\000\009\000\008\000\008\000\
\010\000\010\000\011\000\011\000\013\000\013\000\014\000\014\000\
\015\000\016\000\017\000\017\000\003\000\018\000\018\000\019\000\
\019\000\020\000\020\000\020\000\020\000\020\000\021\000\021\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\000\000\001\000\002\000\004\000\001\000\
\001\000\001\000\001\000\003\000\000\000\001\000\001\000\004\000\
\000\000\001\000\001\000\003\000\000\000\001\000\001\000\003\000\
\004\000\003\000\000\000\002\000\003\000\000\000\001\000\002\000\
\003\000\002\000\003\000\005\000\007\000\009\000\000\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\004\000\003\000\
\002\000"

let yydefred = "\000\000\
\002\000\000\000\065\000\000\000\008\000\009\000\010\000\001\000\
\003\000\000\000\000\000\029\000"

let yydgoto = "\002\000\
\003\000\004\000\009\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yysindex = "\255\255\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\219\254\002\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 292
let yytable = "\001\000\
\008\000\011\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\006\000\
\000\000\000\000\000\000\007\000"

let yycheck = "\001\000\
\000\000\039\001\001\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001\031\001\
\255\255\255\255\255\255\035\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
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
  IF\000\
  ELSE\000\
  FOR\000\
  INT\000\
  BOOL\000\
  VOID\000\
  BUILD\000\
  JOIN\000\
  FRAME\000\
  SET\000\
  EOF\000\
  LTN\000\
  GTN\000\
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
# 37 "src/parser.mly"
            ( _1 )
# 280 "src/parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "src/parser.mly"
                 ( [], []                 )
# 286 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 41 "src/parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 294 "src/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "src/parser.mly"
                  ( []          )
# 300 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 45 "src/parser.mly"
                  ( List.rev _1 )
# 307 "src/parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "src/parser.mly"
                             ( [(_1, _2)]     )
# 315 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "src/parser.mly"
                             ( (_3, _4) :: _1 )
# 324 "src/parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "src/parser.mly"
        ( Int   )
# 330 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "src/parser.mly"
        ( Bool  )
# 336 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "src/parser.mly"
        ( Frame )
# 342 "src/parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 57 "src/parser.mly"
            ([_1])
# 349 "src/parser.ml"
               : 'typedef_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typedef_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 58 "src/parser.mly"
                                 (_3::_1)
# 357 "src/parser.ml"
               : 'typedef_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "src/parser.mly"
                ([])
# 363 "src/parser.ml"
               : 'typedef_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typedef_list) in
    Obj.repr(
# 62 "src/parser.mly"
                   (List.rev _1)
# 370 "src/parser.ml"
               : 'typedef_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "src/parser.mly"
       (
        match _1 with
        | "Int" -> Int
        | "Bool" -> Bool
        | "Void" -> Void
        | "String" -> String
        | "Set" -> failwith ("set map init must with parameters") 
				| x -> Default x 
    )
# 385 "src/parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typedef_list_opt) in
    Obj.repr(
# 75 "src/parser.mly"
                                  (
        match _1 with
        | "Set" -> begin
                match _3 with
                |[x] -> Set x
                | _ -> failwith ("set just with one parameter")
                end
        | "Array" -> begin
               match _3 with
               |[x] -> Array x
               | _ -> failwith ("array just with one parameter")
               end
        | _ -> failwith ("not suppport template except set map")
    )
# 406 "src/parser.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "src/parser.mly"
                        ( [] )
# 412 "src/parser.ml"
               : 'expr_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_true_list_set) in
    Obj.repr(
# 93 "src/parser.mly"
                        ( _1 )
# 419 "src/parser.ml"
               : 'expr_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "src/parser.mly"
                            ( [_1]     )
# 426 "src/parser.ml"
               : 'expr_true_list_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_true_list_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "src/parser.mly"
                                  ( _3 :: _1 )
# 434 "src/parser.ml"
               : 'expr_true_list_set))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "src/parser.mly"
                  ( []          )
# 440 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 102 "src/parser.mly"
                  ( List.rev _1 )
# 447 "src/parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "src/parser.mly"
                            ( [_1]     )
# 454 "src/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "src/parser.mly"
                            ( _3 :: _1 )
# 462 "src/parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list_set) in
    Obj.repr(
# 109 "src/parser.mly"
                                    (Set(List.rev _3))
# 469 "src/parser.ml"
               : 'set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list_set) in
    Obj.repr(
# 111 "src/parser.mly"
                                (Array (List.rev _2))
# 476 "src/parser.ml"
               : 'arr))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "src/parser.mly"
                     ( []       )
# 482 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 115 "src/parser.mly"
                     ( _2 :: _1 )
# 490 "src/parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 118 "src/parser.mly"
               ( (_1, _2) )
# 498 "src/parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "src/parser.mly"
                ([])
# 504 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_true_list) in
    Obj.repr(
# 122 "src/parser.mly"
                     (_1)
# 511 "src/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 125 "src/parser.mly"
              ([_1])
# 518 "src/parser.ml"
               : 'stmt_true_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_true_list) in
    Obj.repr(
# 126 "src/parser.mly"
                               (_1 :: _3)
# 526 "src/parser.ml"
               : 'stmt_true_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 129 "src/parser.mly"
                                            ( Expr _1               )
# 533 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 130 "src/parser.mly"
                                            ( Block(List.rev _2)    )
# 540 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 131 "src/parser.mly"
                                            ( If(_3, _5, Block([])) )
# 548 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 132 "src/parser.mly"
                                            ( If(_3, _5, _7)        )
# 557 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 134 "src/parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 567 "src/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "src/parser.mly"
                  ( Noexpr )
# 573 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "src/parser.mly"
                  ( _1     )
# 580 "src/parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 141 "src/parser.mly"
                                 ( Literal(_1)            )
# 587 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "src/parser.mly"
                                 ( Id(_1)                 )
# 594 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "src/parser.mly"
                                 ( BoolLit(true)          )
# 600 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "src/parser.mly"
                                 ( BoolLit(false)         )
# 606 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set) in
    Obj.repr(
# 145 "src/parser.mly"
                                 ( _1                     )
# 613 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arr) in
    Obj.repr(
# 146 "src/parser.mly"
                                 ( _1                     )
# 620 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "src/parser.mly"
                                 ( Binop(_1, Add,     _3) )
# 628 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "src/parser.mly"
                                 ( Binop(_1, Sub,     _3) )
# 636 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "src/parser.mly"
                                 ( Binop(_1, Mult,    _3) )
# 644 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "src/parser.mly"
                                 ( Binop(_1, Div,     _3) )
# 652 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "src/parser.mly"
                                 ( Binop(_1, Equal,   _3) )
# 660 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "src/parser.mly"
                                 ( Binop(_1, Neq,     _3) )
# 668 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "src/parser.mly"
                                 ( Binop(_1, Less,    _3) )
# 676 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "src/parser.mly"
                                 ( Binop(_1, Leq,     _3) )
# 684 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "src/parser.mly"
                                 ( Binop(_1, Greater, _3) )
# 692 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "src/parser.mly"
                                 ( Binop(_1, Geq,     _3) )
# 700 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "src/parser.mly"
                                 ( Binop(_1, FrameEq, _3) )
# 708 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "src/parser.mly"
                                 ( Binop(_1, And,     _3) )
# 716 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "src/parser.mly"
                                 ( Binop(_1, Or,      _3) )
# 724 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "src/parser.mly"
                                 ( Unop(Neg, _2)          )
# 731 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "src/parser.mly"
                                 ( Unop(Not, _2)          )
# 738 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "src/parser.mly"
                                 ( Assign(_1, _3)         )
# 746 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 163 "src/parser.mly"
                                 ( Call(_1, _3)           )
# 754 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 164 "src/parser.mly"
                                 ( _2                     )
# 761 "src/parser.ml"
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
