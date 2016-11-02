(* ocamllex parser for Blox *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }  (* Whitespace *) 
| "/ * "    { comment lexbuf }           (* Comments   *) 
| '('       { LPAREN  } 
| '='       { ASSIGN  } 
| ')'       { RPAREN  } 
| "=="      { EQ      } 
| '{'       { LBRACE  } 
| "!="      { NEQ     } 
| '}'       { RBRACE  } 
| '<'       { LT      } 
| ';'       { SEMI    } 
| "<="      { LEQ     } 
| ','       { COMMA   } 
| ">"       { GT      } 
| '+'       { PLUS    } 
| ">="      { GEQ     } 
| '-'       { MINUS   } 
| "&&"      { AND     } 
| '*'       { TIMES   } 
| "||"      { OR      } 
| '/'       { DIVIDE  } 
| ".="      { BLOCKEQ }
| "!"       { NOT     } 
| "Create"  { CREATE  }
| "Build"   { BUILD   }
| "Join"    { JOIN    }
| "Frame"   { FRAME   }
| "if"      { IF      } 
| "else"    { ELSE    } 
| "for"     { FOR     } 
| "while"   { WHILE   } 
| "return"  { RETURN  } 
| "int"     { INT     }
| "bool"    { BOOL    } 
| "true"    { TRUE    } 
| "false"   { FALSE   } 
| "void"    { VOID    } 
| "break"   { BREAK   }  
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) } 
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_'] * as lxm { ID(lxm) } 
| eof { EOF } 
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse 
" * /" { token lexbuf   } 
| _    { comment lexbuf }
