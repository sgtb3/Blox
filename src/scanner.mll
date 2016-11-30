{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf   }    (* Whitespace *)
| "/* "                { comment lexbuf }    (* Comments   *)
| '='                  { ASSIGN         }
| '<'                  { FOPEN          }
| '>'                  { FCLOSE         }
| ','                  { COMMA          }
| ';'                  { SEMI           }
| '{'                  { LCURL          }
| '}'                  { RCURL          }
| '('                  { LPAREN         }
| ')'                  { RPAREN         }
| "Join"               { JOIN           }
| "Frame"              { FRAME          }
| "Print"              { PRINT          }
| ['0'-'9']+ as lxm    { INTLIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_'] * as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
" */" { token lexbuf   }
| _    { comment lexbuf }
