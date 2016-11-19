(* ocamllex scanner for Blox *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }  (* Whitespace *) 
| "/ * "     { comment lexbuf }          (* Comments   *) 
| '('        { LPAREN   } 
| '='        { ASSIGN   } 
| ')'        { RPAREN   } 
| "=="       { EQ       } 
| "!="       { NEQ      } 
| '{'        { LBRACE   } 
| '}'        { RBRACE   } 
| '['        { LBRACK  } (*add [] for array init*)
| ']'        { RBRACK  } 
| '<'        { LT       } 
| ';'        { SEMI     } 
| "@"        { AT       }
| "<="       { LEQ      } 
| ','        { COMMA    } 
| ">"        { GT       } 
| '+'        { PLUS     } 
| ">="       { GEQ      } 
| '-'        { MINUS    }
| "&&"       { AND      } 
| '*'        { TIMES    }
| "||"       { OR       } 
| '/'        { DIVIDE   }
| ':'        { COLON    }
| ".="       { FRAMEEQ  }
| '!'        { NOT      }
| '.'        { DOT      }
| "Set"      { SET      }
| "Build"    { BUILD    }
| "Join"     { JOIN     }
| "Frame"    { FRAME    }
| "if"       { IF       } 
| "else"     { ELSE     } 
| "for"      { FOR      } 
| "int"      { INT      }
| "bool"     { BOOL     } 
| "true"     { TRUE     } 
| "false"    { FALSE    } 
| "void"     { VOID     }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) } 
| ['\"'] [^'\"']* ['\"'] as lxm { STRING(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_'] * as lxm { ID(lxm) }
| eof { EOF } 
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse 
" * /" { token lexbuf   } 
| _    { comment lexbuf }
