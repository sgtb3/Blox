(* ocamllex Scanner for Blox *)

{ open Parser }

rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf }    (* Whitespace *) 
| "/ * "     { comment lexbuf }          (* Comments   *) 
| '('        { LPAREN   } 
| ')'        { RPAREN   } 
| '<'        { LT       } 
| ">"        { GT       }
| '='        { ASSIGN   } 
| ','        { COMMA    } 
| ';'        { SEMI     } 
| "Join"     { JOIN     }
| "Frame"    { FRAME    }
| "Print"    { PRINT    }
| "int"      { INT      }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) } 
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_'] * as lxm { ID(lxm) }
| eof { EOF } 
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
and comment = parse 
" * /" { token lexbuf   } 
| _    { comment lexbuf }

(* 
	| "=="       { EQ       } 
	| "!="       { NEQ      } 	
	| '{'        { LBRACE   } 
	| '}'        { RBRACE   } 
	| '['        { LBRACK   }
	| ']'        { RBRACK   } 
	| "@"        { AT       }
	| "<="       { LEQ      }  
	| '+'        { PLUS     } 
	| ">="       { GEQ      } 
	| '-'        { MINUS    }
	| "&&"       { AND      } 
	| '*'        { TIMES    }
	| "||"       { OR       } 
	| '/'        { DIVIDE   }
	| ".="       { FRAMEEQ  }
	| '!'        { NOT      }
	| '.'        { DOT      }
	| "Set"      { SET      }
	| "Rule"	 { RULE		}
	| "Build"    { BUILD    }
	| "if"       { IF       } 
	| "else"     { ELSE     } 
	| "for"      { FOR      } 
	| "bool"     { BOOL     } 
	| "true"     { TRUE     } 
	| "false"    { FALSE    } 
*)
